! { dg-do run }
! Test the fix for PR31197 and PR31258 in which the substrings below
! would cause ICEs because the character lengths were never resolved.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk> 
!            and Thomas Koenig <tkoenig@gcc.gnu.org>
!
  CHARACTER(LEN=3), DIMENSION(10) :: Z
  CHARACTER(LEN=3), DIMENSION(3,3) :: W
  integer :: ctr = 0
  call test_reshape
  call test_eoshift
  call test_cshift
  call test_spread
  call test_transpose
  call test_pack
  call test_unpack
  call test_pr31197
  if (ctr .ne. 8) STOP 1
contains
  subroutine test_reshape 
    Z(:)="123"
    if (any (RESHAPE(Z(:)(2:2),(/5,2/)) .ne. "2")) STOP 2
    ctr = ctr + 1
  end subroutine
  subroutine test_eoshift 
    CHARACTER(LEN=1), DIMENSION(10) :: chk
    chk(1:8) = "5"
    chk(9:10) = " "
    Z(:)="456"
    if (any (EOSHIFT(Z(:)(2:2),2) .ne. chk)) STOP 3
    ctr = ctr + 1
  END subroutine
  subroutine test_cshift 
    Z(:)="901"
    if (any (CSHIFT(Z(:)(2:2),2) .ne. "0")) STOP 4
    ctr = ctr + 1
  end subroutine
  subroutine test_spread 
    Z(:)="789"
    if (any (SPREAD(Z(:)(2:2),dim=1,ncopies=2) .ne. "8")) STOP 5
    ctr = ctr + 1
  end subroutine
  subroutine test_transpose 
    W(:, :)="abc"
    if (any (TRANSPOSE(W(:,:)(1:2)) .ne. "ab")) STOP 6
    ctr = ctr + 1
  end subroutine
  subroutine test_pack 
    W(:, :)="def"
    if (any (pack(W(:,:)(2:3),mask=.true.) .ne. "ef")) STOP 7
    ctr = ctr + 1
  end subroutine
  subroutine test_unpack 
    logical, dimension(5,2) :: mask
    Z(:)="hij"
    mask = .true.
    if (any (unpack(Z(:)(2:2),mask,' ') .ne. "i")) STOP 8
    ctr = ctr + 1
  end subroutine
  subroutine test_pr31197
    TYPE data
      CHARACTER(LEN=3) :: A = "xyz"
    END TYPE
    TYPE(data), DIMENSION(10), TARGET :: T
    if (any (TRANSPOSE(RESHAPE(T(:)%A(2:2),(/5,2/))) .ne. "y")) STOP 9
    ctr = ctr + 1
  end subroutine
END
