// { dg-do compile }
class GrBufferAllocPool {
  virtual ~GrBufferAllocPool();
};
GrBufferAllocPool::~GrBufferAllocPool() { static long a; }

