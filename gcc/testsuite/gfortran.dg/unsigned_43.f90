! { dg-do run }
! { dg-options -funsigned }
program memain
  implicit none
  call tst_1_1
  call tst_1_2
  call tst_1_4
  call tst_1_8
  call tst_2_1
  call tst_2_2
  call tst_2_4
  call tst_2_8
  call tst_4_1
  call tst_4_2
  call tst_4_4
  call tst_4_8
  call tst_8_1
  call tst_8_2
  call tst_8_4
  call tst_8_8
contains
  subroutine tst_1_1
    unsigned(kind=1) :: x, r1, r2, r3, r4
    unsigned(kind=1) :: n
    x = 0u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_1_1
  subroutine tst_1_2
    unsigned(kind=1) :: x, r1, r2, r3, r4
    unsigned(kind=2) :: n
    x = 0u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_1_2
  subroutine tst_1_4
    unsigned(kind=1) :: x, r1, r2, r3, r4
    unsigned(kind=4) :: n
    x = 0u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_1_4
  subroutine tst_1_8
    unsigned(kind=1) :: x, r1, r2, r3, r4
    unsigned(kind=8) :: n
    x = 0u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 0u_1 ** n
    r4 = 0u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 1u_1 ** n
    r4 = 1u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 2u_1 ** n
    r4 = 2u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 3u_1 ** n
    r4 = 3u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 7u_1 ** n
    r4 = 7u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 8u_1 ** n
    r4 = 8u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 9u_1 ** n
    r4 = 9u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 14u_1 ** n
    r4 = 14u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 17u_1 ** n
    r4 = 17u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_1
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 254u_1 ** n
    r4 = 254u_1 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_1_8
  subroutine tst_2_1
    unsigned(kind=2) :: x, r1, r2, r3, r4
    unsigned(kind=1) :: n
    x = 0u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_2_1
  subroutine tst_2_2
    unsigned(kind=2) :: x, r1, r2, r3, r4
    unsigned(kind=2) :: n
    x = 0u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_2_2
  subroutine tst_2_4
    unsigned(kind=2) :: x, r1, r2, r3, r4
    unsigned(kind=4) :: n
    x = 0u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_2_4
  subroutine tst_2_8
    unsigned(kind=2) :: x, r1, r2, r3, r4
    unsigned(kind=8) :: n
    x = 0u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 0u_2 ** n
    r4 = 0u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 1u_2 ** n
    r4 = 1u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 2u_2 ** n
    r4 = 2u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 3u_2 ** n
    r4 = 3u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 7u_2 ** n
    r4 = 7u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 8u_2 ** n
    r4 = 8u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 9u_2 ** n
    r4 = 9u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 14u_2 ** n
    r4 = 14u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 17u_2 ** n
    r4 = 17u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 254u_2 ** n
    r4 = 254u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_2
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 65535u_2 ** n
    r4 = 65535u_2 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_2_8
  subroutine tst_4_1
    unsigned(kind=4) :: x, r1, r2, r3, r4
    unsigned(kind=1) :: n
    x = 0u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_4_1
  subroutine tst_4_2
    unsigned(kind=4) :: x, r1, r2, r3, r4
    unsigned(kind=2) :: n
    x = 0u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_4_2
  subroutine tst_4_4
    unsigned(kind=4) :: x, r1, r2, r3, r4
    unsigned(kind=4) :: n
    x = 0u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_4_4
  subroutine tst_4_8
    unsigned(kind=4) :: x, r1, r2, r3, r4
    unsigned(kind=8) :: n
    x = 0u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 0u_4 ** n
    r4 = 0u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 1u_4 ** n
    r4 = 1u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 2u_4 ** n
    r4 = 2u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 3u_4 ** n
    r4 = 3u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 7u_4 ** n
    r4 = 7u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 8u_4 ** n
    r4 = 8u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 9u_4 ** n
    r4 = 9u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 14u_4 ** n
    r4 = 14u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 17u_4 ** n
    r4 = 17u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 254u_4 ** n
    r4 = 254u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 65535u_4 ** n
    r4 = 65535u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_4
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 4294967295u_4 ** n
    r4 = 4294967295u_4 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_4_8
  subroutine tst_8_1
    unsigned(kind=8) :: x, r1, r2, r3, r4
    unsigned(kind=1) :: n
    x = 0u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 0u_1
    r1 = x ** n
    r2 = x ** 0u_1
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 0u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 1u_1
    r1 = x ** n
    r2 = x ** 1u_1
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 1u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 2u_1
    r1 = x ** n
    r2 = x ** 2u_1
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 2u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 7u_1
    r1 = x ** n
    r2 = x ** 7u_1
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 7u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 8u_1
    r1 = x ** n
    r2 = x ** 8u_1
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 8u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 16u_1
    r1 = x ** n
    r2 = x ** 16u_1
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 16u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 135u_1
    r1 = x ** n
    r2 = x ** 135u_1
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 135u_1
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_8_1
  subroutine tst_8_2
    unsigned(kind=8) :: x, r1, r2, r3, r4
    unsigned(kind=2) :: n
    x = 0u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 0u_2
    r1 = x ** n
    r2 = x ** 0u_2
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 0u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 1u_2
    r1 = x ** n
    r2 = x ** 1u_2
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 1u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 2u_2
    r1 = x ** n
    r2 = x ** 2u_2
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 2u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 7u_2
    r1 = x ** n
    r2 = x ** 7u_2
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 7u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 8u_2
    r1 = x ** n
    r2 = x ** 8u_2
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 8u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 16u_2
    r1 = x ** n
    r2 = x ** 16u_2
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 16u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 135u_2
    r1 = x ** n
    r2 = x ** 135u_2
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 135u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 32779u_2
    r1 = x ** n
    r2 = x ** 32779u_2
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 32779u_2
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_8_2
  subroutine tst_8_4
    unsigned(kind=8) :: x, r1, r2, r3, r4
    unsigned(kind=4) :: n
    x = 0u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 0u_4
    r1 = x ** n
    r2 = x ** 0u_4
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 0u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 1u_4
    r1 = x ** n
    r2 = x ** 1u_4
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 1u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 2u_4
    r1 = x ** n
    r2 = x ** 2u_4
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 2u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 7u_4
    r1 = x ** n
    r2 = x ** 7u_4
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 7u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 8u_4
    r1 = x ** n
    r2 = x ** 8u_4
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 8u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 16u_4
    r1 = x ** n
    r2 = x ** 16u_4
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 16u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 135u_4
    r1 = x ** n
    r2 = x ** 135u_4
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 135u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 32779u_4
    r1 = x ** n
    r2 = x ** 32779u_4
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 32779u_4
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_8_4
  subroutine tst_8_8
    unsigned(kind=8) :: x, r1, r2, r3, r4
    unsigned(kind=8) :: n
    x = 0u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 0u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 0u_8 ** n
    r4 = 0u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 1u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 1u_8 ** n
    r4 = 1u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 2u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 2u_8 ** n
    r4 = 2u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 3u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 3u_8 ** n
    r4 = 3u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 7u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 7u_8 ** n
    r4 = 7u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 8u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 8u_8 ** n
    r4 = 8u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 9u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 9u_8 ** n
    r4 = 9u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 14u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 14u_8 ** n
    r4 = 14u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 17u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 17u_8 ** n
    r4 = 17u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 254u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 254u_8 ** n
    r4 = 254u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 65535u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 65535u_8 ** n
    r4 = 65535u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 4294967295u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 4294967295u_8 ** n
    r4 = 4294967295u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 0u_8
    r1 = x ** n
    r2 = x ** 0u_8
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 0u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 1u_8
    r1 = x ** n
    r2 = x ** 1u_8
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 1u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 2u_8
    r1 = x ** n
    r2 = x ** 2u_8
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 2u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 7u_8
    r1 = x ** n
    r2 = x ** 7u_8
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 7u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 8u_8
    r1 = x ** n
    r2 = x ** 8u_8
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 8u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 16u_8
    r1 = x ** n
    r2 = x ** 16u_8
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 16u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 135u_8
    r1 = x ** n
    r2 = x ** 135u_8
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 135u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 32779u_8
    r1 = x ** n
    r2 = x ** 32779u_8
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 32779u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

    x = 18446744073709551615u_8
    n = 18446744073709551615u_8
    r1 = x ** n
    r2 = x ** 18446744073709551615u_8
    r3 = 18446744073709551615u_8 ** n
    r4 = 18446744073709551615u_8 ** 18446744073709551615u_8
    if (r1 /= r2 .or. r3 /= r4 .or. r1 /= r3) error stop

  end subroutine tst_8_8
end program memain
