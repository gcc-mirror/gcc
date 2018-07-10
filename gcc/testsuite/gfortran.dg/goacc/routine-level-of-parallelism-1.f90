! Test various aspects of clauses specifying compatible levels of
! parallelism with the OpenACC routine directive.  The Fortran counterpart is
! c-c++-common/goacc/routine-level-of-parallelism-2.c

subroutine g_1
  !$acc routine gang
end subroutine g_1

subroutine s_1_2a
  !$acc routine
end subroutine s_1_2a

subroutine s_1_2b
  !$acc routine seq
end subroutine s_1_2b

subroutine s_1_2c
  !$acc routine (s_1_2c)
end subroutine s_1_2c

subroutine s_1_2d
  !$acc routine (s_1_2d) seq
end subroutine s_1_2d

module s_2
contains
  subroutine s_2_1a
    !$acc routine
  end subroutine s_2_1a

  subroutine s_2_1b
    !$acc routine seq
  end subroutine s_2_1b

  subroutine s_2_1c
    !$acc routine (s_2_1c)
  end subroutine s_2_1c

  subroutine s_2_1d
    !$acc routine (s_2_1d) seq
  end subroutine s_2_1d
end module s_2

subroutine test
  external g_1, w_1, v_1
  external s_1_1, s_1_2

  interface
     function s_3_1a (a)
       integer a
       !$acc routine
     end function s_3_1a
  end interface

  interface
     function s_3_1b (a)
       integer a
       !$acc routine seq
     end function s_3_1b
  end interface

  !$acc routine(g_1) gang

  !$acc routine(w_1) worker

  !$acc routine(v_1) worker

  ! Also test the implicit seq clause.

  !$acc routine (s_1_1) seq

end subroutine test
