// DR2327: In direct-initialization, prefer a constructor even if it requires a
// qualification conversion.

// { dg-do compile { target c++11 } }

struct Dog;
struct Cat {
  Cat(const Dog&);
};
struct Dog {
  operator Cat() = delete;
};

Cat cat(Dog{});
