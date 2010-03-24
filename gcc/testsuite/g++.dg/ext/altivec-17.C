// { dg-do compile { target powerpc*-*-* } }
// { dg-require-effective-target powerpc_altivec_ok }
// { dg-options "-maltivec" }

// Make sure that bool vectors have distinct names to int vectors

#define vector__ __attribute__((altivec (vector__)))
#define bool__ __attribute__((altivec(bool__)))

typedef vector__ unsigned int simd_type;
typedef vector__ bool__ int bool_simd_type;

void Foo (bool_simd_type const &a)
{
  simd_type const &v = a; // { dg-error "'const __vector.4. unsigned int&' from expression of type 'const __vector.4. __bool int'" }
}
