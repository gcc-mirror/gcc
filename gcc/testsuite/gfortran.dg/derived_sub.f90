! { dg-do compile }
! PR35475  gfortran fails to compile valid code with ICE error in fold-const.c
! Test case from PR report added to avoid future regression
module modone
  type mytype
    real :: myvar
  end type
end module

module modtwo
  interface
    subroutine subone(mytype_cur)
      use modone
      type (mytype) mytype_cur
    end subroutine
  end interface

contains

  subroutine subtwo(mytype_cur)
    use modone
    type (mytype) mytype_cur,mytype_fin
    mytype_fin=mytype_cur
    return
  end subroutine

  subroutine subthree(mytype_cur)
    use modone
    type (mytype) mytype_cur
    call subone(mytype_cur)
  end subroutine 

end module
! { dg-final { cleanup-modules "modone modtwo" } }
