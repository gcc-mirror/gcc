// { dg-do compile }
// { dg-options "-Wframe-address" }

static void* const fa[] = {
  __builtin_frame_address (0),
  __builtin_frame_address (1), // { dg-warning "builtin_frame_address" }
  __builtin_frame_address (2), // { dg-warning "builtin_frame_address" }
  __builtin_frame_address (3), // { dg-warning "builtin_frame_address" }
  __builtin_frame_address (4)  // { dg-warning "builtin_frame_address" }
};


static void* const ra[] = {
  __builtin_return_address (0),
  __builtin_return_address (1), // { dg-warning "builtin_return_address" }
  __builtin_return_address (2), // { dg-warning "builtin_return_address" }
  __builtin_return_address (3), // { dg-warning "builtin_return_address" }
  __builtin_return_address (4)  // { dg-warning "builtin_return_address" }
};


void* __attribute__ ((noclone, noinline))
test_builtin_frame_address (unsigned i)
{
  void* const fa[] = {
    __builtin_frame_address (0),
    __builtin_frame_address (1), // { dg-warning "builtin_frame_address" }
    __builtin_frame_address (2), // { dg-warning "builtin_frame_address" }
    __builtin_frame_address (3), // { dg-warning "builtin_frame_address" }
    __builtin_frame_address (4)  // { dg-warning "builtin_frame_address" }
  };

  return fa [i];
}


void* __attribute__ ((noclone, noinline))
test_builtin_return_address (unsigned i)
{
  void* const ra[] = {
    __builtin_return_address (0),
    __builtin_return_address (1), // { dg-warning "builtin_return_address" }
    __builtin_return_address (2), // { dg-warning "builtin_return_address" }
    __builtin_return_address (3), // { dg-warning "builtin_return_address" }
    __builtin_return_address (4)  // { dg-warning "builtin_return_address" }
  };
  return ra [i];
}


int main ()
{
  test_builtin_frame_address (0);

  test_builtin_return_address (0);

  void* const a[] = {
    __builtin_frame_address (0),
    __builtin_frame_address (1), // { dg-warning "builtin_frame_address" }
    __builtin_frame_address (2), // { dg-warning "builtin_frame_address" }
    __builtin_frame_address (3), // { dg-warning "builtin_frame_address" }
    __builtin_frame_address (4), // { dg-warning "builtin_frame_address" }

    __builtin_return_address (0),
    __builtin_return_address (1), // { dg-warning "builtin_return_address" }
    __builtin_return_address (2), // { dg-warning "builtin_return_address" }
    __builtin_return_address (3), // { dg-warning "builtin_return_address" }
    __builtin_return_address (4)  // { dg-warning "builtin_return_address" }
  };
}
