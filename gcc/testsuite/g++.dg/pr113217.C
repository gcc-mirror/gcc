// { dg-do compile }
// { dg-options "-O -g -fnon-call-exceptions" }
struct _Vector_base {
  int _M_end_of_storage;
};
struct vector : _Vector_base {
  vector() : _Vector_base() {}
  ~vector();
};
struct LoadGraph {
  LoadGraph();
  vector colors;
  vector data_block;
};
LoadGraph::LoadGraph() {}
