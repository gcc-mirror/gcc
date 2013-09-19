// PR c++/58457

struct allocator
{
  void operator delete (void*);
  void* operator new (__SIZE_TYPE__, void*);
};

struct type : public allocator
{
  type() {}
  using allocator::operator new;
  using allocator::operator delete;
};

int main()
{
  new (0) type;
  return 0;
}
