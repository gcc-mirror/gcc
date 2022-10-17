/* Make sure that OpenACC loop processing happens.  */
/* { dg-additional-options "-O2 -fdump-tree-oaccloops" } */

extern int place ();

void vector_1 (int *ary, int size)
{
#pragma acc parallel num_workers (32) vector_length(32) copy(ary[0:size]) firstprivate (size)
  {
#pragma acc loop gang
    for (int jx = 0; jx < 1; jx++)
#pragma acc loop auto
      for (int ix = 0; ix < size; ix++)
	ary[ix] = place ();
  }
}

/* { dg-final { scan-tree-dump {OpenACC loops.*Loop 0\(0\).*Loop 44\(1\).*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_HEAD_MARK, 0, 1, 68\);.*Head-0:.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_HEAD_MARK, 0, 1, 68\);.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_FORK, \.data_dep\.[0-9_]+, 0\);.*Tail-0:.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_TAIL_MARK, \.data_dep\.[0-9_]+, 1\);.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_JOIN, \.data_dep\.[0-9_]+, 0\);.*Loop 6\(6\).*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_HEAD_MARK, 0, 2, 6\);.*Head-0:.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_HEAD_MARK, 0, 2, 6\);.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_FORK, \.data_dep\.[0-9_]+, 1\);.*Head-1:.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_HEAD_MARK, \.data_dep\.[0-9_]+, 1\);.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_FORK, \.data_dep\.[0-9_]+, 2\);.*Tail-1:.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_TAIL_MARK, \.data_dep\.[0-9_]+, 2\);.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_JOIN, \.data_dep\.[0-9_]+, 2\);.*Tail-0:.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_TAIL_MARK, \.data_dep\.[0-9_]+, 1\);.*\.data_dep\.[0-9_]+ = \.UNIQUE \(OACC_JOIN, \.data_dep\.[0-9_]+, 1\);} "oaccloops" } } */
