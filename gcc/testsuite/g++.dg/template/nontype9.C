// { dg-do compile  }
// { dg-prune-output "mangled name" }
// Contributed by: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
int i;

template <void (&FN)()>
struct g {
  void foo(void) {
    FN ();
  }
};

void h ()
{
  i = 7;
}

template struct g<h>;

