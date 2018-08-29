// { dg-do compile }

// Avoid -pedantic-error default
// { dg-options "" }

void a() {
  a[0](); // { dg-warning "arithmetic" }
}
