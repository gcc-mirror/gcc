// { dg-do compile }
// { dg-options "-gcodeview" }

// get_type_num_struct must defer inner before revisiting its anonymous parent.

struct outer
{
  struct
  {
    struct inner
    {
      int i;
    };

    inner value;
  } member;
};

outer o;
