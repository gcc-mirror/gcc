! { dg-do compile }
! Checks the fix for PR34672, in which generic interfaces were not
! being written correctly, when renamed.
!
! Contributed by Jos de Kloe <kloedej@knmi.nl> 
!
MODULE MyMod1
  integer, parameter :: i2_ = Selected_Int_Kind(4)
END Module MyMod1

module MyMod2
  INTERFACE write_int
     module procedure write_int_local
  END INTERFACE
contains
  subroutine write_int_local(value)
    integer, intent(in)  :: value
    print *,value
  end subroutine write_int_local
end module MyMod2

module MyMod3
  USE MyMod2, only: write_MyInt   => write_int
  USE MyMod1, only: i2_
end module MyMod3

module MyMod4
  USE MyMod3, only: write_MyInt
end module MYMOD4
