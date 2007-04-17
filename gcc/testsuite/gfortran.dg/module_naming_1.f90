! { dg-do assemble }
! PR 31144
! Makes sure that our name mangling scheme can't be outwitted

! old scheme
module m1
contains
  subroutine m2__m3()
  end subroutine m2__m3
end module m1

module m1__m2
contains
  subroutine m3()
  end subroutine m3
end module m1__m2

! New scheme, relies on capitalization
module m2
contains
  subroutine m2_MOD_m3()
    ! mangled to __m2_MOD_m2_mod_m3
  end subroutine m2_MOD_m3
end module m2

module m2_MOD_m2
contains
  subroutine m3()
    ! mangled to __m2_mod_m2_MOD_m3
  end subroutine m3
end module m2_MOD_m2
