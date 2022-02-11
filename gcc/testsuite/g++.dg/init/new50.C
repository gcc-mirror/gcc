// PR c++/104084

int nothrow;
struct MaxAlignedAllocable {
  void *operator new[](__SIZE_TYPE__, int);
  void operator delete[](void *);
  long Resize_size;
  void Resize() { new (nothrow) MaxAlignedAllocable[Resize_size]; }
};
