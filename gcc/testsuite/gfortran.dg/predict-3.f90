! { dg-do compile }
! { dg-options "-fno-tree-fre -fno-tree-ccp -Og" }

program simplify_transfer
  call pr30881 ()
contains
  subroutine pr18769 ()
    type t
    end type t
  end subroutine pr18769
  subroutine pr30881 ()
    INTEGER, PARAMETER :: K=1
    I=TRANSFER(.TRUE.,K)
    SELECT CASE(I)
      CASE(TRANSFER(.TRUE.,K))
      CASE(TRANSFER(.FALSE.,K))
        STOP 2
      CASE DEFAULT
        STOP 3
    END SELECT
  END subroutine pr30881
  subroutine pr31194 ()
  end subroutine pr31194
  subroutine pr31216 ()
  END subroutine pr31216
  subroutine pr31427 ()
  END subroutine pr31427
end program simplify_transfer
