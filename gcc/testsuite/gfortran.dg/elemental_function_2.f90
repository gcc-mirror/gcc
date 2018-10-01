! { dg-do compile }
!
! Test the fix for PR87239 in which the call to the elemental function
! 'gettwo' was being added before the scalarization loop in the assignment.
! Since the result temporary was being declared in the loop body, this
! drove the gimplifier crazy. It is sufficient to compile this testcase
! since it used to ICE.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module test
  implicit none
contains

  elemental function gettwo( s ) result( res )
    character(*), intent(in) :: s
    character(len(s)) :: res

    res = s( 1 : 2 )
  endfunction gettwo

endmodule test

program main
  use test
  implicit none
  character(10) :: inp( 5 )
  integer :: i

  ! character(10), allocatable :: out(:) ! this works
  character(:), allocatable :: out(:) ! this was stuffed

  inp = [ 'aaa', 'bbb', 'ccc', 'ddd', 'eee' ]

  out = gettwo( inp )

  do i = 1, size (out, 1)
    if (trim (out(i)) .ne. inp(i)(1:2)) stop 1
  end do
endprogram main
