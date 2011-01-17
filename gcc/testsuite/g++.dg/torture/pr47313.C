// { dg-do compile }

namespace internal {
    template < class DSC, bool Const >   struct CC_iterator   {
	typedef CC_iterator iterator;
	typedef typename DSC::value_type value_type;
	typedef const value_type* pointer;
	CC_iterator ()     ;
	CC_iterator (const iterator &it)     {
	}
	pointer p;
	pointer operator->() const ;
    };
}
template < class T > struct Compact_container {
    typedef Compact_container <T> Self;
    typedef T value_type;
    typedef internal::CC_iterator<Self, false> iterator;
};
template < typename TDS = void > struct Periodic_3_triangulation_ds_cell_base_3 {
    typedef typename TDS::Vertex_handle Vertex_handle;
    const Vertex_handle& vertex(int i) const   {
    }
};
struct Triangulation_data_structure_3    {
    typedef Triangulation_data_structure_3 Tds;
    typedef Periodic_3_triangulation_ds_cell_base_3<Tds> Cell;
    typedef Compact_container<Cell> Cell_range;
    typedef Compact_container<int> Vertex_range;
    typedef typename Cell_range::iterator Cell_handle;
    typedef typename Vertex_range::iterator Vertex_handle;
};
typedef Triangulation_data_structure_3 TDS1;
template <  class > struct Periodic_3_Delaunay_triangulation_3 {
    typedef TDS1::Vertex_handle Vertex_handle;
    typedef TDS1::Cell_handle Cell_handle;
    int compare_distance() const {
    }
    Vertex_handle nearest_vertex() const;
};
template < class Tds > typename Periodic_3_Delaunay_triangulation_3<Tds>::Vertex_handle Periodic_3_Delaunay_triangulation_3<Tds>::nearest_vertex() const {
    Cell_handle c ;
    Vertex_handle nearest = c->vertex(0);
    nearest = (compare_distance() == -1) ?       nearest : c->vertex(0);
    return nearest;
}
typedef Periodic_3_Delaunay_triangulation_3<TDS1> PDT1;
struct Periodic_3_triangulation_hierarchy_3   : PDT1 {
    Vertex_handle   nearest_vertex() const;
};
Periodic_3_triangulation_hierarchy_3::Vertex_handle Periodic_3_triangulation_hierarchy_3:: nearest_vertex() const {
    return PDT1::nearest_vertex();
}
