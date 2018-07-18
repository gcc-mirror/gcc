// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

namespace std {
template<class,class>struct pair{};
  template<typename _Tp> struct _Vector_base {
      struct _Vector_impl
      {
	_Tp* _M_start;
	_Tp* _M_finish;
	_Tp* _M_end_of_storage;
      };
      _Vector_impl _M_impl;
    };
  template<typename _Tp >
    struct vector : _Vector_base<_Tp>
    {
      vector(const vector& __x);
    };
}
namespace boost {
struct G{};
template <class T, class U >
struct modable2
: G
{ };
}
namespace CGAL {
struct Rep { };
struct Handle
{
    Handle() ;
    Handle(const Handle& x) ;
    Rep* PTR;
};
template <typename ET_> class Lazy_exact_nt
  : Handle
  , boost::modable2< Lazy_exact_nt<ET_>, int >
  , boost::modable2< Lazy_exact_nt<ET_>, double >
{ };
  struct CC_iterator { };
struct Triangulation_data_structure_3 {
  typedef CC_iterator                          Vertex_handle;
  typedef CC_iterator                            Cell_handle;
  typedef std::pair<Cell_handle, int>              Facet;
};
template < class GT, class Tds_ > struct Triangulation_3 {
  typedef Tds_                                  Tds;
  typedef typename GT::Point_3                 Point;
  typedef typename Tds::Facet                  Facet;
  typedef typename Tds::Vertex_handle          Vertex_handle;
  enum Locate_type { VERTEX=0, EDGE, FACET, CELL, OUTSIDE_CONVEX_HULL, OUTSIDE_AFFINE_HULL };
  Tds _tds;
  bool is_infinite(const Facet & f) const ;
};
template < class Gt, class Tds_ > struct Delaunay_triangulation_3 : public Triangulation_3<Gt, Tds_> { };
  namespace Surface_mesher { enum Verbose_flag { VERBOSE, NOT_VERBOSE }; }
enum Mesher_level_conflict_status { NO_CONFLICT = 0, CONFLICT_BUT_ELEMENT_CAN_BE_RECONSIDERED, CONFLICT_AND_ELEMENT_SHOULD_BE_DROPPED };
struct Null_mesher_level {
  template <typename P, typename Z> Mesher_level_conflict_status test_point_conflict_from_superior(P, Z) ;
};
template < class Tr, class Derived, class Element, class Previous, class Triangulation_traits > struct Mesher_level {
  typedef Tr Triangulation;
  typedef typename Triangulation::Point Point;
  typedef typename Triangulation::Vertex_handle Vertex_handle;
  typedef typename Triangulation_traits::Zone Zone;
  typedef Previous Previous_level;
  Derived& derived() { return static_cast<Derived&>(*this); }
  Previous& previous_level;
  Mesher_level(Previous_level& previous)
    : previous_level(previous)
  { }
  Vertex_handle insert(Point p, Zone& z) ; // { dg-warning "used but never defined" }
  Zone conflicts_zone(const Point& p, Element e) ; // { dg-warning "used but never defined" }
  Element get_next_element() ; // { dg-warning "used but never defined" }
  template <class Mesh_visitor> void before_insertion(Element& e, const Point& p, Zone& zone, Mesh_visitor visitor) {
    visitor.before_insertion(e, p, zone);
  }
  template <class Mesh_visitor> void after_insertion(Vertex_handle vh, Mesh_visitor visitor) {
    derived().after_insertion_impl(vh);
  }
  template <class Mesh_visitor> void after_no_insertion(const Element& e, const Point& p, Zone& zone, Mesh_visitor visitor) {
    visitor.after_no_insertion(e, p, zone);
  }
  template <class Mesh_visitor> void refine(Mesh_visitor visitor)
  {
    Element e = get_next_element();
    const Mesher_level_conflict_status result = try_to_refine_element(e, visitor);
  }
  template <class Mesh_visitor> Mesher_level_conflict_status try_to_refine_element(Element e, Mesh_visitor visitor)
  {
    Point p ;
    Zone zone = conflicts_zone(p, e);
    const Mesher_level_conflict_status result = test_point_conflict(p, zone);
      before_insertion(e, p, zone, visitor);
      Vertex_handle v = insert(p, zone);
      after_insertion(v, visitor);
      after_no_insertion(e, p, zone, visitor);
  }
  Mesher_level_conflict_status test_point_conflict(const Point& p, Zone& zone)
  {
    return previous_level.test_point_conflict_from_superior(p, zone);
  }
};
struct Null_mesh_visitor {
  template <typename E, typename P, typename Z> void before_insertion(E, P, Z) const {}
  template <typename E, typename P, typename Z> void after_no_insertion(E, P, Z) const {}
};
template <class Tr> struct Triangulation_ref_impl {
  Triangulation_ref_impl(Tr& t);
};
template <typename Tr> struct Triangulation_mesher_level_traits_3
: public Triangulation_ref_impl<Tr>
{
  typedef typename Tr::Facet Facet;
  Triangulation_mesher_level_traits_3(Tr& t)
    : Triangulation_ref_impl<Tr>(t)
  { }
  struct Zone {
    typedef std::vector<int*> Cells;
    typedef std::vector<Facet> Facets;
    typedef typename Tr::Locate_type Locate_type;
    Locate_type locate_type;
    Cells cells;
    Facets boundary_facets;
    Facets internal_facets;
  };
};
  namespace Surface_mesher {
    namespace details {
      template <typename Base> struct Triangulation_generator {
        typedef typename Base::Complex_2_in_triangulation_3 C2T3;
        typedef typename C2T3::Triangulation Triangulation;
	typedef Triangulation Type;
	typedef Type type;
      };
      template <typename Base> struct Facet_generator {
        typedef typename Triangulation_generator<Base>::type Tr;
	typedef typename Tr::Facet Type;
	typedef Type type;
      };
      template <typename Base, typename Self, typename Element, typename PreviousLevel = Null_mesher_level> struct Mesher_level_generator {
        typedef typename Base::Complex_2_in_triangulation_3 C2T3;
        typedef typename C2T3::Triangulation Triangulation;
        typedef Triangulation_mesher_level_traits_3<Triangulation> Tr_m_l_traits_3;
        typedef Mesher_level <Triangulation, Self, Element, PreviousLevel, Tr_m_l_traits_3> Type;
      };
    }
  template < class C2T3, class Surface_, class SurfaceMeshTraits, class Criteria_ > struct Surface_mesher_base
    : public Triangulation_mesher_level_traits_3<typename C2T3::Triangulation>
  {
    typedef C2T3 Complex_2_in_triangulation_3;
    typedef Surface_ Surface;
    typedef SurfaceMeshTraits Surface_mesh_traits;
    typedef Criteria_ Criteria;
    typedef typename C2T3::Triangulation Tr;
    typedef typename Tr::Vertex_handle Vertex_handle;
    typedef typename Tr::Facet Facet;
    Surface_mesher_base (C2T3& co, const Surface& s, const Surface_mesh_traits& mesh_traits, const Criteria& c)
: Triangulation_mesher_level_traits_3<Tr>(co.triangulation()), c2t3(co), tr(co.triangulation()), surf(s), meshtraits(mesh_traits), criteria(c)
    { }
    C2T3& c2t3;
    Tr& tr;
    const Surface& surf;
    const Surface_mesh_traits& meshtraits;
    const Criteria& criteria;
    void after_insertion_impl(const Vertex_handle& v) {
	after_insertion_handle_opposite_facet (Facet ());
	after_insertion_handle_incident_facet (Facet ());
    }
    void after_insertion_handle_incident_facet (const Facet& f) {
      tr.is_infinite(f) ;
      new_facet<false>(f);
    }
    template <bool remove_from_complex_if_not_in_restricted_Delaunay> void new_facet (const Facet& f) ; // { dg-warning "used but never defined" }
    void after_insertion_handle_opposite_facet (const Facet& f) {
      after_insertion_handle_incident_facet (f);
    }
  };
  template < typename Base, typename Element = typename details::Facet_generator<Base>::type, typename PreviousLevel = Null_mesher_level, Verbose_flag verbose = NOT_VERBOSE > struct Surface_mesher
    : public Base , public details::Mesher_level_generator< Base, Surface_mesher<Base, Element, PreviousLevel, verbose>, Element, PreviousLevel >::Type
  {
    typedef typename Base::Complex_2_in_triangulation_3 C2T3;
    typedef typename Base::Surface Surface;
    typedef typename Base::Criteria Criteria;
    typedef typename Base::Surface_mesh_traits Surface_mesh_traits;
    typedef typename details::Mesher_level_generator< Base, Surface_mesher<Base, Element, PreviousLevel, verbose>, Element, PreviousLevel >::Type Mesher_lvl;
    using Mesher_lvl::refine;
    Null_mesher_level null_mesher_level;
    Null_mesh_visitor null_visitor;
    bool initialized;
    Surface_mesher(C2T3& c2t3, const Surface& surface, const Surface_mesh_traits& mesh_traits, const Criteria& criteria)
      : Base(c2t3, surface, mesh_traits, criteria), Mesher_lvl(null_mesher_level), initialized(false)
    { }
    void refine_mesh () {
      refine(null_visitor);
    }
  };
  }
template <typename Surface> struct Surface_mesh_traits_generator_3 {
  typedef typename Surface::Surface_mesher_traits_3 Type;
  typedef Type type;
};
template < class Tr, typename Edge_info_ = void > struct Complex_2_in_triangulation_3 {
  typedef Tr Triangulation;
  Triangulation& triangulation();
};
template <class Tr> struct Surface_mesh_complex_2_in_triangulation_3
: public Complex_2_in_triangulation_3<Tr>
{ };
  struct Non_manifold_tag {};
  template < typename C2T3, typename SurfaceMeshTraits_3, typename Criteria, typename Tag > struct Make_surface_mesh_helper {
    typedef Surface_mesher::Surface_mesher_base< C2T3, typename SurfaceMeshTraits_3::Surface_3, SurfaceMeshTraits_3, Criteria> Mesher_base;
  };
  template <typename C2T3, typename SurfaceMeshTraits_3, typename Criteria, typename Tag, Surface_mesher::Verbose_flag verbosity = Surface_mesher::NOT_VERBOSE > struct Surface_mesher_generator {
    typedef typename Make_surface_mesh_helper< C2T3, SurfaceMeshTraits_3, Criteria, Tag>::Mesher_base Mesher_base;
    typedef Surface_mesher::Surface_mesher< Mesher_base, typename Surface_mesher::details::Facet_generator<Mesher_base>::type, Null_mesher_level, verbosity> Mesher;
    typedef Mesher type;
  };
template <typename C2T3, typename SurfaceMeshTraits_3, typename Criteria> void make_surface_mesh(C2T3& c2t3, const typename SurfaceMeshTraits_3::Surface_3& surface, const SurfaceMeshTraits_3& surface_mesh_traits, const Criteria& criteria) {
  typedef typename Surface_mesher_generator< C2T3, SurfaceMeshTraits_3, Criteria, Non_manifold_tag, Surface_mesher::NOT_VERBOSE >::type Mesher;
  Mesher mesher(c2t3, surface, surface_mesh_traits, criteria);
  mesher.refine_mesh();
}
template <class Kernel> struct Surface_mesh_triangulation_generator_3 {
  typedef CGAL::Triangulation_data_structure_3 Tds;
  typedef CGAL::Delaunay_triangulation_3<Kernel, Tds> Type;
};
  namespace Surface_mesher {
  namespace { struct Return_min { }; }
  template < class GT, class Surface, class Unused = Return_min > struct Implicit_surface_oracle_3 {
    typedef Surface Surface_3;
  };
  }
  template< typename GT> struct Implicit_surface_3 {
    typedef GT Geom_traits;
    typedef Implicit_surface_3<Geom_traits > Self;
    typedef Surface_mesher::Implicit_surface_oracle_3< Geom_traits, Self> Surface_mesher_traits_3;
  };
}
struct K {
struct Point_3 {
CGAL::Lazy_exact_nt<double> a[3];
};
};
typedef CGAL::Surface_mesh_triangulation_generator_3<K>::Type Tr;
typedef CGAL::Surface_mesh_complex_2_in_triangulation_3<Tr> C2T3;
typedef CGAL::Implicit_surface_3<K > Surface;
typedef CGAL::Surface_mesh_traits_generator_3<Surface>::type Traits;
void f() {
	C2T3 c2t3 ;
	CGAL::make_surface_mesh(c2t3, Surface(), Traits(), 3);
}
