// PR c++/50930
// { dg-do compile { target c++11 } }

struct nmc {
 nmc() = default;
 nmc(nmc&&) = delete; // line 3
};

struct A { // line 6
 nmc n{};
 nmc n2 = {};
} a; // line 8

// ------

struct lock_t {
  int lock[4];
};

struct pthread_mutex_t {
  volatile lock_t __spinlock;
};

struct mutex {
  pthread_mutex_t m = { };
  mutex() = default;
};

int main()
{
  mutex mx;
}
