// PR c++/105852
// { dg-additional-options -w }

template <class> struct Local { friend Local False(int *); };
Local<int> loc;
Local<int> False(int *);
void New() { False; }
Local<int> False(int *) { return Local<int>(); }
