! Program to test FORALL with scalar pointer assignment inside it.
program forall_6
  type element
    real, pointer :: p
  end type

  type (element) q(5)
  real, target, dimension(5) :: t
  integer i;

  t = (/1.0, 2.0, 3.0, 4.0, 5.0/)

  do i = 1,5
    q(i)%p => t(i)
  end do

  forall (i = 1:5)
    q(i)%p => q(6 - i)%p
  end forall


  do i = 1,5
    if (q(i)%p .ne. t(6 - i)) call abort
  end do
end
