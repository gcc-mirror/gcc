! { dg-do run }
! PR 25292: Check that the intrinsic associated works with functions returning
! pointers as arguments
program test
   real, pointer :: a, b

   nullify(a,b)
   if(associated(a,b).or.associated(a,a)) call abort()
   allocate(a)
   if(associated(b,a)) call abort()
   if (.not.associated(x(a))) call abort ()
   if (.not.associated(a, x(a))) call abort ()

   nullify(b)
   if (associated(x(b))) call abort ()
   allocate(b)
   if (associated(x(b), x(a))) call abort ()

contains

  function x(a) RESULT(b)
    real, pointer :: a,b
    b => a
  end function x

end program test
