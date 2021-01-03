// PR c++/80891 part 4

// ICE copying an augmented lookup during ADL

struct __normal_iterator get(); // { dg-message "candidate: .__normal_iterator get\\(\\)." }
namespace boost {
template <class> void get(); // { dg-message "candidate: .template<class> void boost::get\\(\\)." }
struct A {
  A(int);
};
enum problem_selector { subgraph_iso };
template <typename, typename, typename, typename,
          typename SubGraphIsoMapCallback, problem_selector>
struct B {
  B(A, A, int, int, int, int);
  void m_fn1(SubGraphIsoMapCallback p1) {
    __normal_iterator __trans_tmp_1(); // { dg-warning "empty parentheses" }
    p1(__trans_tmp_1, 0);
  }
};
template <typename Graph1, typename Graph2, typename IndexMap1,
          typename IndexMap2, typename VertexOrder1,
          typename EdgeEquivalencePredicate,
          typename VertexEquivalencePredicate, typename SubGraphIsoMapCallback,
          problem_selector problem_selection>
void match(
    Graph1, Graph2, SubGraphIsoMapCallback p3, VertexOrder1,
    B<IndexMap1, IndexMap2, EdgeEquivalencePredicate,
      VertexEquivalencePredicate, SubGraphIsoMapCallback, problem_selection>
        p5) {
  p5.m_fn1(p3);
}
template <problem_selector problem_selection, typename GraphSmall,
          typename GraphLarge, typename IndexMapSmall, typename IndexMapLarge,
          typename VertexOrderSmall, typename EdgeEquivalencePredicate,
          typename VertexEquivalencePredicate, typename SubGraphIsoMapCallback>
void vf2_subgraph_morphism(GraphSmall, GraphLarge, SubGraphIsoMapCallback p3,
                           IndexMapSmall, IndexMapLarge, VertexOrderSmall,
                           EdgeEquivalencePredicate,
                           VertexEquivalencePredicate) {
  B<IndexMapSmall, IndexMapLarge, EdgeEquivalencePredicate,
    VertexEquivalencePredicate, SubGraphIsoMapCallback, problem_selection>
      s(0, 0, 0, 0, 0, 0);
  match(0, 0, p3, 0, s);
}
template <typename GraphSmall, typename GraphLarge, typename IndexMapSmall,
          typename IndexMapLarge, typename VertexOrderSmall,
          typename EdgeEquivalencePredicate,
          typename VertexEquivalencePredicate, typename SubGraphIsoMapCallback>
int vf2_subgraph_iso(GraphSmall, GraphLarge, SubGraphIsoMapCallback p3,
                     IndexMapSmall, IndexMapLarge, VertexOrderSmall,
                     EdgeEquivalencePredicate, VertexEquivalencePredicate) {
  vf2_subgraph_morphism<subgraph_iso>(0, 0, p3, 0, 0, 0, 0, 0);
  return 0;
}
}
using namespace boost;
struct C {
  C(int) : graph1_(0), graph2_(0) {}
  template <typename CorrespondenceMap1To2, typename CorrespondenceMap2To1>
  void operator()(CorrespondenceMap1To2 p1, CorrespondenceMap2To1) {
    get(p1); // { dg-error "no matching function" }
  }
  A graph1_;
  A graph2_;
};
template <typename> void get(); // { dg-message "candidate: .template<class> void get\\(\\)." }
void test_vf2_sub_graph_iso() { C a(vf2_subgraph_iso(0, 0, a, 0, 0, 0, 0, 0));
}
