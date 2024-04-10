// P0847R7
// { dg-do compile { target c++23 } }

// lambda declaration with xobj parameter

struct S{};

void test()
{
  (void)[](this auto&& self){};
  (void)[](this auto& self){};
  (void)[](this auto const& self){};
  (void)[](this auto self){};

  (void)[](this S&& self){};
  (void)[](this S& self){};
  (void)[](this S const& self){};
  (void)[](this S self){};

  (void)[x = 0](this auto&& self){};
  (void)[x = 0](this auto& self){};
  (void)[x = 0](this auto const& self){};
  (void)[x = 0](this auto self){};
}

