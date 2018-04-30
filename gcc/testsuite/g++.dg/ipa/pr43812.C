/* { dg-do compile } */
/* { dg-options "-O -fwhole-program -fipa-cp" } */

typedef float scoord_t;
typedef scoord_t sdist_t;
typedef sdist_t dist_t;
template<typename T> class TRay { };
typedef TRay<dist_t> Ray;
class BBox { };
class RenderContext { };
class RefCounted {
public:
    void deref () const {
        if (--ref_count <= 0) {
            delete this;
        }
    }
    mutable int ref_count;
};
template<class T> class Ref {
public:
    ~Ref () {
        if (obj) obj->deref ();
    }
    T *obj;
};
class Material : public RefCounted { };
class Surface {
public:
    virtual ~Surface () { }
    class IsecInfo   { };
    virtual const IsecInfo *intersect (Ray &ray, RenderContext &context) const;
    Ref<const Material> material;
};
class LocalSurface : public Surface {
    virtual BBox bbox () const;
};
BBox LocalSurface::bbox () const { return BBox(); }
