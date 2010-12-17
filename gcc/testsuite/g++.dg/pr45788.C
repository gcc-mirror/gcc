// { dg-do compile { target x86_64-*-* } }
// { dg-options "-O3 -fwhole-program -msse2" }

typedef long unsigned int __darwin_size_t;
typedef __darwin_size_t size_t;

typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
typedef float __v4sf __attribute__ ((__vector_size__ (16)));
extern inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__)) _mm_mul_ps (__m128 __A, __m128 __B) {
    return (__m128) __builtin_ia32_mulps ((__v4sf)__A, (__v4sf)__B);
}
extern inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__)) _mm_set1_ps (float __F) {
    return __extension__ (__m128)(__v4sf){
	__F, __F, __F, __F };
}
extern inline __m128 __attribute__((__gnu_inline__, __always_inline__, __artificial__)) _mm_setr_ps (float __Z, float __Y, float __X, float __W) {
}
typedef float real;
template <typename T, int N> struct vectorX {
};
template<> struct vectorX<float, 3> {
    union {
	__m128 s;
	struct {
	};
    };
    vectorX(__m128 s) : s(s) {
    }

}
__attribute__((aligned));
template<> struct vectorX<float, 4> {
    typedef float T;
    typedef vectorX<float, 4> V;
    union {
	__m128 s;
	struct {
	    T r, g, b, a;
	};
    };
    vectorX(T a_, T b, T c, T d = 1) : s(_mm_setr_ps(a_,b,c,d)) {
    }
    vectorX(__m128 s) : s(s) {
    }
    vectorX(const V &t) : s(t.s) {
    }
    V &operator *=(const T t) {
	s = _mm_mul_ps(s, _mm_set1_ps(t));
	return *this;
    }
    inline V operator *(const T t) const __attribute__((always_inline)) {
	return V(*this) *= t;
    };
}
__attribute__((aligned));
typedef vectorX<real, 3> color3;
typedef vectorX<real, 4> color4;
typedef color3 color;
static inline color4 c3to4(color c) {
    color4 res(c.s);
    res.a=1;
    return res;
}
static inline color c4to3(color4 c) {
    return color(c.s);
}
static inline color4 to_premultiplied(color c, real a) {
    color4 res = c3to4(c);
    return res * a;
}
static inline color4 to_premultiplied(color4 cs) {
    return to_premultiplied(c4to3(cs), cs.a);
}
struct texture {
};
struct flat_texture : public texture {
    color4 c;
    flat_texture(const color4 &c) : c(to_premultiplied(c)) {
    }
};
struct checkerboard_texture : public texture {
    color4 even, odd;
    checkerboard_texture(const color4 &even, const color4 &odd) : even(to_premultiplied(even)), odd(to_premultiplied(odd)) {
    }
};
struct texture_placement {
    texture *tex;
};
struct surface {
    texture_placement textures[16];
    size_t texcount;
};
struct primitive {
    surface mat;
};
static void set_color(primitive *p, color4 c) {
    p->mat.textures[0].tex = new flat_texture(c);
}
static primitive **checkerboard_scene(int *pi) {
    primitive **prims = new primitive*[6];
    set_color(prims[0], color4(.7,.7,.7));
    prims[1]->mat.textures[prims[1]->mat.texcount++].tex = new checkerboard_texture(color4(1,.1,.1),color4(.1,.15,1));
    set_color(prims[2], color4(.7,.9,.7));
}
int main (int argc, char * const argv[]) {
    int primi;
    primitive **prims = checkerboard_scene(&primi);
}
