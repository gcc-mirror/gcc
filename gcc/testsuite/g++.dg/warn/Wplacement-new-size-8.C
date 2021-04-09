/* Verify informational notes following the warning.
   { dg-do compile }
   { dg-options "-Wall" } */

#define DISS_MAX  __PTRDIFF_MAX__
#define SIZE_MAX  __SIZE_MAX__

typedef __SIZE_TYPE__  size_t;

template <int N> struct S { char a[N]; };

void* operator new (size_t, void *p) { return p; }
void* operator new[] (size_t, void *p) { return p; }


void test_cst_off ()
{
  {
    char ca0[0];                // { dg-message "'ca0' declared here" "note" }
    new (ca0 + 0) S<1>;         // { dg-warning "constructing an object of type 'S<1>' and size '1' in a region of type 'char \\\[0]' and size '0'" }
  }
  {
    char ca1[1];
    new (ca1 + 0) S<1>;
  }
  {
    char ca1[1];                // { dg-message "'ca1' declared here" "note" }
    new (ca1 + 0) S<2>;         // { dg-warning "constructing an object of type 'S<2>' and size '2' in a region of type 'char \\\[1]' and size '1'" }
  }
  {
    char ca1[1];                // { dg-message "at offset 1 from 'ca1' declared here" "note" }
    new (ca1 + 1) S<1>;         // { dg-warning "constructing an object of type 'S<1>' and size '1' in a region of type 'char \\\[1]' and size '0'" }
  }
  {
    char ca1[1];                // { dg-message "at offset 2 from 'ca1' declared here" "note" }
    new (ca1 + 2) S<1>;         // { dg-warning "constructing an object of type 'S<1>' and size '1' in a region of type 'char \\\[1]' and size '0'" }
  }
  {
    char ca1[1];                // { dg-message "at offset -1 from 'ca1' declared here" "note" }
    new (ca1 - 1) S<1>;         // { dg-warning "constructing an object of type 'S<1>' and size '1' in a region of type 'char \\\[1]' and size '0'" }
  }
  {
    /* Offsets are treated as signed so SIZE_MAX is indistinguishable
       from -1.  */
    char ca1[1];                // { dg-message "at offset \\d+ from 'ca1' declared here" "note" { xfail *-*-* } }
                                // { dg-message "at offset -1 from 'ca1' declared here" "note second variant" { target *-*-* } .-1 }
    new (ca1 + SIZE_MAX) S<1>;  // { dg-warning "constructing an object of type 'S<1>' and size '1' in a region of type 'char \\\[1]' and size '0'" }
  }
}


/* Verify that the range of the offset included in the note corresponds
   to the range of its type (plus the optional constant).  */

void test_var_off_uchar (unsigned char i)
{
  {
    // Verify that the nore doesn't mention an offset.
    char ca0[0];                // { dg-message ": 'ca0' declared here" "note" }
    new (ca0 + i) S<1>;         // { dg-warning "constructing an object of type 'S<1>' and size '1' in a region of type 'char \\\[0]' and size '0'" }
  }
  {
    char ca1[1];
    new (ca1 + i) S<1>;
  }
  {
    // Verify that the nore doesn't mention an offset.
    char ca1[1];                // { dg-message ": 'ca1' declared here" "note" }
    new (ca1 + i) S<2>;         // { dg-warning "constructing an object of type 'S<2>' and size '2' in a region of type 'char \\\[1]' and size at most '1'" }
  }
  {
    char ca2[2];
    new (ca2 + i) S<2>;
    new (ca2 + 1 - i) S<2>;
    new (ca2 - i + 1) S<2>;
    new (ca2 - 2 + i) S<2>;
    new (ca2 - i + 2) S<2>;
    new (ca2 - i + i) S<2>;
    new (ca2 + i + i) S<2>;
  }
  {
    char ca2[2];                // { dg-message "at offset \\\[1, 2] from 'ca2' declared here" "note" }
    new (ca2 + i + 1) S<2>;     // { dg-warning "constructing an object of type 'S<2>' and size '2' in a region of type 'char \\\[2]' and size at most '1'" }
  }

  {
    char a[65281];
    new (a + i + 65280) S<1>;
  }
  {
    char a[65281];              // { dg-message "at offset 65281 from 'a' declared here" "note" }
    new (a + i + 65281) S<1>;   // { dg-warning "constructing an object of type 'S<1>' and size '1' in a region of type 'char \\\[65281]' and size '0'" }
  }
  {
    char a[65281];              // { dg-message "at offset \\\[65154, 65281] from 'a' declared here" "note" }
    new (a + i + 65154) S<128>; // { dg-warning "constructing an object of type 'S<128>' and size '128' in a region of type 'char \\\[65281]' and size at most '127'" }
  }
}


/* Same as above but also verify that the signedness of the offset is
   considered in the issuing the warning.  */

void test_var_off_schar (signed char i)
{
  {
    // Verify that the nore doesn't mention an offset.
    char ca0[0];                // { dg-message ": 'ca0' declared here" "note" }
    new (ca0 + i) S<1>;         // { dg-warning "constructing an object of type 'S<1>' and size '1' in a region of type 'char \\\[0]' and size '0'" }
  }
  {
    char ca1[1];
    new (ca1 + i) S<1>;
    new (ca1 - i) S<1>;
    new (ca1 + i + 1) S<1>;
    new (ca1 - i + 1) S<1>;
    new (ca1 + i + i) S<1>;
    new (ca1 - i - i) S<1>;
  }
  {
    // Verify that the nore doesn't mention an offset.
    char ca1[1];                // { dg-message ": 'ca1' declared here" "note" }
    new (ca1 + i) S<2>;         // { dg-warning "constructing an object of type 'S<2>' and size '2' in a region of type 'char \\\[1]' and size at most '1'" }
  }
  {
    char ca2[2];
    new (ca2 + i) S<2>;
    new (ca2 + 1 - i) S<2>;
    new (ca2 - i + 1) S<2>;
    new (ca2 - 2 + i) S<2>;
    new (ca2 - i + 2) S<2>;
    new (ca2 - i + i) S<2>;
    new (ca2 + i + i) S<2>;
  }
  {
    char ca2[2];
    new (ca2 + i + 1) S<2>;
  }

  {
    char a[65281];              // { dg-message "at offset \\\[65153, 65281] from 'a'" "note" }
    new (a + i + 65280) S<1>;
    new (a + i + 65281) S<1>;
    new (a + i + 65281) S<128>;
    new (a + i + 65281) S<129>; // { dg-warning "constructing an object of type 'S<129>' and size '129' in a region of type 'char \\\[65281]' and size at most '128'" }
  }
}
