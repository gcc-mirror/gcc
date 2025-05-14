// Tests of dynamic exception specifications
// { dg-require-effective-target c++14_down }
// { dg-prune-output "dynamic exception specifications are deprecated" }

struct io_error {};
struct file_io_error : public io_error {};
struct mem_error {};

// Valid intraprocedural:

void test_1 (int flag) throw (io_error)
{
    if (flag)
      throw io_error();
}

// Invalid intraprocedural:

void test_2 (int flag) throw (io_error) // { dg-warning "throwing exception of unexpected type 'mem_error' from 'test_2'" }
// { dg-message "exception of unexpected type 'mem_error' thrown from 'test_2'" "" { target *-*-* } .-1 }
// { dg-message "'test_2' declared here" "" { target *-*-* } .-2 }
{
    if (flag)
      throw mem_error(); // { dg-message "throwing exception of type 'mem_error' here\.\.\." }
}

// Valid intraprocedural with subclass:

void test_3 (int flag) throw (io_error) // { dg-bogus "throwing exception of unexpected type 'file_io_error' from 'test_3'" "PR analyzer/119697" { xfail *-*-* } }
{
    if (flag)
      throw file_io_error();
}

// Valid interprocedural:

void test_4_inner (int flag)
{
  if (flag)
    throw io_error ();
}

void test_4_outer (int flag) throw (io_error)
{
  test_4_inner (flag);
}

// Invalid interprocedural:

void test_5_inner (int flag)
{
  if (flag)
    throw mem_error (); // { dg-message "throwing exception of type 'mem_error' here\.\.\." }
  // { dg-message "unwinding stack frame" "" { target *-*-* } .-1 }
}

void test_5_outer (int flag) throw (io_error) // { dg-warning "throwing exception of unexpected type 'mem_error' from 'test_5_outer'" }
// { dg-message "exception of unexpected type 'mem_error' thrown from 'test_5_outer'" "" { target *-*-* } .-1 }
// { dg-message "'test_5_outer' declared here" "" { target *-*-* } .-2 }
{
  test_5_inner (flag);
}
