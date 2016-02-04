// PR c++/68597
// { dg-do compile { target c++14 } }

auto make_test_objective3(double beta) {
  auto phi0_prime = [=](double alpha) {
    if (alpha <= 1-beta)
      return -1;
    else if (alpha >= 1+beta)
      return 1;
    else
      return 1 / beta * (alpha - 1); // { dg-error "type" }
  };
}
