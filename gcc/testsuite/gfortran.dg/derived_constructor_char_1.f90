! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/44857
!
!
  Type :: t5
    character (len=5) :: txt(2)
  End Type t5

  character (len=3), parameter :: str3(2) = [ "ABC", "ZYX" ]
  character (len=5), parameter :: str5(2) = [ "AbCdE", "ZyXwV" ]
  character (len=5), parameter :: str7(2) = [ "aBcDeFg", "zYxWvUt" ]

  Type (t5) :: one   = t5((/ "12345", "67890" /))
  Type (t5) :: two   = t5((/ "123", "678" /))
  Type (t5) :: three = t5((/ "1234567", "abcdefg" /))
  Type (t5) :: four  = t5(str3)
  Type (t5) :: five  = t5(str5)
  Type (t5) :: six  = t5(str7)
  print '(2a)', one, two, three, four, five, six
End

subroutine wasICEing()
  implicit none

  Type :: Err_Text_Type
    integer :: nlines
    character (len=132), dimension(5) :: txt
  End Type Err_Text_Type

  Type (Err_Text_Type)  :: Mess_FindFMT =  &
                                Err_Text_Type(0, (/" "," "," "," "," "/))
end subroutine wasICEing

subroutine anotherCheck()
  Type :: t
    character (len=3) :: txt(2)
  End Type
  Type (t) :: tt = t((/ character(len=5) :: "12345", "67890" /))
  print *, tt
end subroutine

! { dg-final { scan-tree-dump-times "one = ..txt=..12345., .67890...;" 1 "original" } }
! { dg-final { scan-tree-dump-times "two = ..txt=..123  ., .678  ...;" 1 "original" } }
! { dg-final { scan-tree-dump-times "three = ..txt=..12345., .abcde...;" 1 "original" } }
! { dg-final { scan-tree-dump-times "four = ..txt=..ABC  ., .ZYX  ...;" 1 "original" } }
! { dg-final { scan-tree-dump-times "five = ..txt=..AbCdE., .ZyXwV...;" 1 "original" } }
! { dg-final { scan-tree-dump-times "six = ..txt=..aBcDe., .zYxWv...;" 1 "original" } }
