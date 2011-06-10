// { dg-options "" }

class A
{
  static const _Complex double x = 1.0 + 2.0i;
};

// { dg-prune-output "constexpr. needed" }
