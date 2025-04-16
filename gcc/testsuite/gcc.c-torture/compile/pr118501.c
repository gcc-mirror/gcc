struct s1 {
  double data[2];
};
double h(double t, struct s1 z_) {
  return z_.data[1] * __builtin_copysign(1.0, t);
}
