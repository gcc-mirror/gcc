/* { dg-do compile } */

typdef int my_int; // { dg-error "1: 'typdef' does not name a type; did you mean 'typedef'\\?" }
inlien int i_fn(); // { dg-error "1: 'inlien' does not name a type; did you mean 'inline'\\?" }
coonst int ci = 1; // { dg-error "1: 'coonst' does not name a type; did you mean 'const'\\?" }
voltil int vi = 2; // { dg-error "1: 'voltil' does not name a type; did you mean 'volatile'\\?" }

class my_class {
  explict my_class(int); // { dg-error "3: 'explict' does not name a type; did you mean 'explicit'\\?" }
  friends double f_fn(); // { dg-error "3: 'friends' does not name a type; did you mean 'friend'\\?" }
  virtial double v_fn(); // { dg-error "3: 'virtial' does not name a type; did you mean 'virtual'\\?" }
};
