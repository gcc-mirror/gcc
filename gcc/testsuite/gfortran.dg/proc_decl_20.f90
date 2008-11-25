! { dg-do compile }
!
! PR fortran/36463
! Gfortran used to fail on this testcase with: 
! gfc_get_default_type(): Bad symbol '@0'
!
! Original program by James Van Buskirk
! Reduced by Janus Weil <janus@gcc.gnu.org>

module other_fun
   interface
      function abstract_fun(x)
         integer x
         integer abstract_fun(x)
      end function abstract_fun
   end interface
end module other_fun

 program fptr
    use other_fun
    procedure(abstract_fun) :: fun
 end program fptr

! { dg-final { cleanup-modules "other_fun" } }
