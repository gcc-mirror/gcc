! { dg-do run }
!
!
program test
  implicit none
  call char_test()
contains
subroutine char_test()
  character(len=3, kind=1), save :: str1a[*], str1b(5)[*]
  character(len=7, kind=1), save :: str2a[*], str2b(5)[*]
  character(len=3, kind=4), save :: ustr1a[*], ustr1b(5)[*]
  character(len=7, kind=4), save :: ustr2a[*], ustr2b(5)[*]

  ! ---------- Assign to coindexed variable -------------

  ! - - - - - scalar = scalar

  ! SCALAR - kind 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  str2a = 1_"XXXXXXX"
  if (this_image() == num_images()) then
    str2a[1] = str1a
  end if
  sync all
  if (this_image() == 1) then
    if (str2a /= 1_"abc    ") STOP 1
  else
    if (str2a /= 1_"XXXXXXX") STOP 2
  end if

  ! SCALAR - kind 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  ustr2a = 4_"XXXXXXX"
  if (this_image() == num_images()) then
    ustr2a[1] = ustr1a
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2a /= 4_"abc    ") STOP 3
  else
    if (ustr2a /= 4_"XXXXXXX") STOP 4
  end if

  ! SCALAR - kind 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcde"
  str1a = 1_"XXX"
  if (this_image() == num_images()) then
    str1a[1] = str2a
  end if
  sync all
  if (this_image() == 1) then
    if (str1a /= 1_"abc") STOP 5
  else
    if (str1a /= 1_"XXX") STOP 6
  end if

  ! SCALAR - kind 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcde"
  ustr1a = 4_"XXX"
  if (this_image() == num_images()) then
    ustr1a[1] = ustr2a
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1a /= 4_"abc") STOP 7
  else
    if (ustr1a /= 4_"XXX") STOP 8
  end if

  ! - - - - - array = array

  ! contiguous ARRAY - kind 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1b(1) = 1_"abc"
  str1b(2) = 1_"def"
  str1b(3) = 1_"gjh"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b(:)[1] = str1b
  end if
  sync all
  if (this_image() == 1) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"def    " &
        .or. str2b(3) /= 1_"gjh    ") STOP 9
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 10
  end if

  ! contiguous ARRAY - kind 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1b(1) = 4_"abc"
  ustr1b(2) = 4_"def"
  ustr1b(3) = 4_"gjh"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b(:)[1] = ustr1b
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"def    " &
        .or. ustr2b(3) /= 4_"gjh    ") STOP 11
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 12
  end if

  ! contiguous ARRAY - kind 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2b(1) = 1_"abcdefg"
  str2b(2) = 1_"hijklmn"
  str2b(3) = 1_"opqrstu"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b(:)[1] = str2b
  end if
  sync all
  if (this_image() == 1) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"hij" &
        .or. str1b(3) /= 1_"opq") STOP 13
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 14
  end if

  ! contiguous ARRAY - kind 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2b(1) = 4_"abcdefg"
  ustr2b(2) = 4_"hijklmn"
  ustr2b(3) = 4_"opqrstu"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b(:)[1] = ustr2b
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"hij" &
        .or. ustr1b(3) /= 4_"opq") STOP 15
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 16
  end if

  ! - - - - - array = scalar

  ! contiguous ARRAY - kind 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b(:)[1] = str1a
  end if
  sync all
  if (this_image() == 1) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"abc    " &
        .or. str2b(3) /= 1_"abc    ") STOP 17
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 18
  end if

  ! contiguous ARRAY - kind 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b(:)[1] = ustr1a
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"abc    " &
        .or. ustr2b(3) /= 4_"abc    ") STOP 19
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 20
  end if

  ! contiguous ARRAY - kind 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcdefg"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b(:)[1] = str2a
  end if
  sync all
  if (this_image() == 1) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"abc" &
        .or. str1b(3) /= 1_"abc") STOP 21
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 22
  end if

  ! contiguous ARRAY - kind 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcdefg"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b(:)[1] = ustr2a
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"abc" &
        .or. ustr1b(3) /= 4_"abc") STOP 23
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 24
  end if

  ! ---------- Take from a coindexed variable -------------

  ! - - - - - scalar = scalar

  ! SCALAR - kind 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  str2a = 1_"XXXXXXX"
  if (this_image() == num_images()) then
    str2a = str1a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str2a /= 1_"abc    ") STOP 25
  else
    if (str2a /= 1_"XXXXXXX") STOP 26
  end if

  ! SCALAR - kind 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  ustr2a = 4_"XXXXXXX"
  if (this_image() == num_images()) then
    ustr2a = ustr1a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr2a /= 4_"abc    ") STOP 27
  else
    if (ustr2a /= 4_"XXXXXXX") STOP 28
  end if

  ! SCALAR - kind 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcde"
  str1a = 1_"XXX"
  if (this_image() == num_images()) then
    str1a = str2a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str1a /= 1_"abc") STOP 29
  else
    if (str1a /= 1_"XXX") STOP 30
  end if

  ! SCALAR - kind 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcde"
  ustr1a = 4_"XXX"
  if (this_image() == num_images()) then
    ustr1a = ustr2a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr1a /= 4_"abc") STOP 31
  else
    if (ustr1a /= 4_"XXX") STOP 32
  end if

  ! - - - - - array = array

  ! contiguous ARRAY - kind 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1b(1) = 1_"abc"
  str1b(2) = 1_"def"
  str1b(3) = 1_"gjh"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b = str1b(:)[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"def    " &
        .or. str2b(3) /= 1_"gjh    ") STOP 33
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 34
  end if

  ! contiguous ARRAY - kind 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1b(1) = 4_"abc"
  ustr1b(2) = 4_"def"
  ustr1b(3) = 4_"gjh"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b = ustr1b(:)[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"def    " &
        .or. ustr2b(3) /= 4_"gjh    ") STOP 35
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 36
  end if

  ! contiguous ARRAY - kind 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2b(1) = 1_"abcdefg"
  str2b(2) = 1_"hijklmn"
  str2b(3) = 1_"opqrstu"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b = str2b(:)[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"hij" &
        .or. str1b(3) /= 1_"opq") STOP 37
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 38
  end if

  ! contiguous ARRAY - kind 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2b(1) = 4_"abcdefg"
  ustr2b(2) = 4_"hijklmn"
  ustr2b(3) = 4_"opqrstu"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b = ustr2b(:)[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"hij" &
        .or. ustr1b(3) /= 4_"opq") STOP 39
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 40
  end if

  ! - - - - - array = scalar

  ! contiguous ARRAY - kind 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b = str1a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"abc    " &
        .or. str2b(3) /= 1_"abc    ") STOP 41
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 42
  end if

  ! contiguous ARRAY - kind 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b = ustr1a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"abc    " &
        .or. ustr2b(3) /= 4_"abc    ") STOP 43
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 44
  end if

  ! contiguous ARRAY - kind 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcdefg"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b = str2a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"abc" &
        .or. str1b(3) /= 1_"abc") STOP 45
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 46
  end if

  ! contiguous ARRAY - kind 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcdefg"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b = ustr2a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"abc" &
        .or. ustr1b(3) /= 4_"abc") STOP 47
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 48
  end if


  ! ---------- coindexed to coindexed variable -------------

  ! - - - - - scalar = scalar

  ! SCALAR - kind 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  str2a = 1_"XXXXXXX"
  if (this_image() == num_images()) then
    str2a[1] = str1a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str2a /= 1_"abc    ") STOP 49
  else
    if (str2a /= 1_"XXXXXXX") STOP 50
  end if

  ! SCALAR - kind 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  ustr2a = 4_"XXXXXXX"
  if (this_image() == num_images()) then
    ustr2a[1] = ustr1a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2a /= 4_"abc    ") STOP 51
  else
    if (ustr2a /= 4_"XXXXXXX") STOP 52
  end if

  ! SCALAR - kind 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcde"
  str1a = 1_"XXX"
  if (this_image() == num_images()) then
    str1a[1] = str2a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str1a /= 1_"abc") STOP 53
  else
    if (str1a /= 1_"XXX") STOP 54
  end if

  ! SCALAR - kind 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcde"
  ustr1a = 4_"XXX"
  if (this_image() == num_images()) then
    ustr1a[1] = ustr2a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1a /= 4_"abc") STOP 55
  else
    if (ustr1a /= 4_"XXX") STOP 56
  end if

  ! - - - - - array = array

  ! contiguous ARRAY - kind 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1b(1) = 1_"abc"
  str1b(2) = 1_"def"
  str1b(3) = 1_"gjh"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b(:)[1] = str1b(:)[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"def    " &
        .or. str2b(3) /= 1_"gjh    ") STOP 57
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 58
  end if

  ! contiguous ARRAY - kind 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1b(1) = 4_"abc"
  ustr1b(2) = 4_"def"
  ustr1b(3) = 4_"gjh"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b(:)[1] = ustr1b(:)[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"def    " &
        .or. ustr2b(3) /= 4_"gjh    ") STOP 59
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 60
  end if

  ! contiguous ARRAY - kind 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2b(1) = 1_"abcdefg"
  str2b(2) = 1_"hijklmn"
  str2b(3) = 1_"opqrstu"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b(:)[1] = str2b(:)[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"hij" &
        .or. str1b(3) /= 1_"opq") STOP 61
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 62
  end if

  ! contiguous ARRAY - kind 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2b(1) = 4_"abcdefg"
  ustr2b(2) = 4_"hijklmn"
  ustr2b(3) = 4_"opqrstu"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b(:)[1] = ustr2b(:)[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"hij" &
        .or. ustr1b(3) /= 4_"opq") STOP 63
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 64
  end if

  ! - - - - - array = scalar

  ! contiguous ARRAY - kind 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b(:)[1] = str1a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"abc    " &
        .or. str2b(3) /= 1_"abc    ") STOP 65
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 66
  end if

  ! contiguous ARRAY - kind 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b(:)[1] = ustr1a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"abc    " &
        .or. ustr2b(3) /= 4_"abc    ") STOP 67
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 68
  end if

  ! contiguous ARRAY - kind 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcdefg"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b(:)[1] = str2a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"abc" &
        .or. str1b(3) /= 1_"abc") STOP 69
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 70
  end if

  ! contiguous ARRAY - kind 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcdefg"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b(:)[1] = ustr2a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"abc" &
        .or. ustr1b(3) /= 4_"abc") STOP 71
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 72
  end if

  ! ============== char1 <-> char4 =====================

  ! ---------- Assign to coindexed variable -------------

  ! - - - - - scalar = scalar

  ! SCALAR - kind 1 <- 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  str1a = 1_"XXXXXXX"
  if (this_image() == num_images()) then
    str2a[1] = ustr1a
  end if
  sync all
  if (this_image() == 1) then
    if (str2a /= 1_"abc    ") STOP 73
  else
    if (str2a /= 1_"XXXXXXX") STOP 74
  end if

  ! SCALAR - kind 4 <- 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 4_"abc"
  ustr2a = 1_"XXXXXXX"
  if (this_image() == num_images()) then
    ustr2a[1] = str1a
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2a /= 4_"abc    ") STOP 75
  else
    if (ustr2a /= 4_"XXXXXXX") STOP 76
  end if

  ! SCALAR - kind 1 <- 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcde"
  str1a = 1_"XXX"
  if (this_image() == num_images()) then
    str1a[1] = ustr2a
  end if
  sync all
  if (this_image() == 1) then
    if (str1a /= 1_"abc") STOP 77
  else
    if (str1a /= 1_"XXX") STOP 78
  end if

  ! SCALAR - kind 4 <- 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 4_"abcde"
  ustr1a = 1_"XXX"
  if (this_image() == num_images()) then
    ustr1a[1] = str2a
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1a /= 4_"abc") STOP 79
  else
    if (ustr1a /= 4_"XXX") STOP 80
  end if

  ! - - - - - array = array

  ! contiguous ARRAY - kind 1 <- 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1b(1) = 4_"abc"
  ustr1b(2) = 4_"def"
  ustr1b(3) = 4_"gjh"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b(:)[1] = ustr1b
  end if
  sync all
  if (this_image() == 1) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"def    " &
        .or. str2b(3) /= 1_"gjh    ") STOP 81
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 82
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1b(1) = 1_"abc"
  str1b(2) = 1_"def"
  str1b(3) = 1_"gjh"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b(:)[1] = str1b
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"def    " &
        .or. ustr2b(3) /= 4_"gjh    ") STOP 83
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 84
  end if

  ! contiguous ARRAY - kind 1 <- 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2b(1) = 4_"abcdefg"
  ustr2b(2) = 4_"hijklmn"
  ustr2b(3) = 4_"opqrstu"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b(:)[1] = ustr2b
  end if
  sync all
  if (this_image() == 1) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"hij" &
        .or. str1b(3) /= 1_"opq") STOP 85
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 86
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2b(1) = 1_"abcdefg"
  str2b(2) = 1_"hijklmn"
  str2b(3) = 1_"opqrstu"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b(:)[1] = str2b
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"hij" &
        .or. ustr1b(3) /= 4_"opq") STOP 87
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 88
  end if

  ! - - - - - array = scalar

  ! contiguous ARRAY - kind 1 <- 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b(:)[1] = ustr1a
  end if
  sync all
  if (this_image() == 1) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"abc    " &
        .or. str2b(3) /= 1_"abc    ") STOP 89
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 90
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b(:)[1] = str1a
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"abc    " &
        .or. ustr2b(3) /= 4_"abc    ") STOP 91
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 92
  end if

  ! contiguous ARRAY - kind 1 <- 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcdefg"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b(:)[1] = ustr2a
  end if
  sync all
  if (this_image() == 1) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"abc" &
        .or. str1b(3) /= 1_"abc") STOP 93
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 94
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcdefg"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b(:)[1] = str2a
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"abc" &
        .or. ustr1b(3) /= 4_"abc") STOP 95
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 96
  end if

  ! ---------- Take from a coindexed variable -------------

  ! - - - - - scalar = scalar

  ! SCALAR - kind 1 <- 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  str2a = 1_"XXXXXXX"
  if (this_image() == num_images()) then
    str2a = ustr1a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str2a /= 1_"abc    ") STOP 97
  else
    if (str2a /= 1_"XXXXXXX") STOP 98
  end if

  ! SCALAR - kind 4 <- 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  ustr2a = 4_"XXXXXXX"
  if (this_image() == num_images()) then
    ustr2a = str1a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr2a /= 4_"abc    ") STOP 99
  else
    if (ustr2a /= 4_"XXXXXXX") STOP 100
  end if

  ! SCALAR - kind 1 <- 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcde"
  str1a = 1_"XXX"
  if (this_image() == num_images()) then
    str1a = ustr2a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str1a /= 1_"abc") STOP 101
  else
    if (str1a /= 1_"XXX") STOP 102
  end if

  ! SCALAR - kind 4 <- 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcde"
  ustr1a = 4_"XXX"
  if (this_image() == num_images()) then
    ustr1a = str2a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr1a /= 4_"abc") STOP 103
  else
    if (ustr1a /= 4_"XXX") STOP 104
  end if

  ! - - - - - array = array

  ! contiguous ARRAY - kind 1 <- 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1b(1) = 4_"abc"
  ustr1b(2) = 4_"def"
  ustr1b(3) = 4_"gjh"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b = ustr1b(:)[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"def    " &
        .or. str2b(3) /= 1_"gjh    ") STOP 105
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 106
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1b(1) = 1_"abc"
  str1b(2) = 1_"def"
  str1b(3) = 1_"gjh"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b = str1b(:)[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"def    " &
        .or. ustr2b(3) /= 4_"gjh    ") STOP 107
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 108
  end if

  ! contiguous ARRAY - kind 1 <- 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2b(1) = 4_"abcdefg"
  ustr2b(2) = 4_"hijklmn"
  ustr2b(3) = 4_"opqrstu"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b = ustr2b(:)[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"hij" &
        .or. str1b(3) /= 1_"opq") STOP 109
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 110
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2b(1) = 1_"abcdefg"
  str2b(2) = 1_"hijklmn"
  str2b(3) = 1_"opqrstu"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b = str2b(:)[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"hij" &
        .or. ustr1b(3) /= 4_"opq") STOP 111
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 112
  end if

  ! - - - - - array = scalar

  ! contiguous ARRAY - kind 1 <- 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b = ustr1a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"abc    " &
        .or. str2b(3) /= 1_"abc    ") STOP 113
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 114
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b = str1a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"abc    " &
        .or. ustr2b(3) /= 4_"abc    ") STOP 115
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 116
  end if

  ! contiguous ARRAY - kind 1 <- 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcdefg"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b = ustr2a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"abc" &
        .or. str1b(3) /= 1_"abc") STOP 117
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 118
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcdefg"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b = str2a[1]
  end if
  sync all
  if (this_image() == num_images()) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"abc" &
        .or. ustr1b(3) /= 4_"abc") STOP 119
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 120
  end if


  ! ---------- coindexed to coindexed variable -------------

  ! - - - - - scalar = scalar

  ! SCALAR - kind 1 <- 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  str2a = 1_"XXXXXXX"
  if (this_image() == num_images()) then
    str2a[1] = ustr1a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str2a /= 1_"abc    ") STOP 121
  else
    if (str2a /= 1_"XXXXXXX") STOP 122
  end if

  ! SCALAR - kind 4 <- 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  ustr2a = 4_"XXXXXXX"
  if (this_image() == num_images()) then
    ustr2a[1] = str1a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2a /= 4_"abc    ") STOP 123
  else
    if (ustr2a /= 4_"XXXXXXX") STOP 124
  end if

  ! SCALAR - kind 1 <- 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcde"
  str1a = 1_"XXX"
  if (this_image() == num_images()) then
    str1a[1] = ustr2a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str1a /= 1_"abc") STOP 125
  else
    if (str1a /= 1_"XXX") STOP 126
  end if

  ! SCALAR - kind 4 <- 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcde"
  ustr1a = 4_"XXX"
  if (this_image() == num_images()) then
    ustr1a[1] = str2a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1a /= 4_"abc") STOP 127
  else
    if (ustr1a /= 4_"XXX") STOP 128
  end if

  ! - - - - - array = array

  ! contiguous ARRAY - kind 1 <- 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1b(1) = 4_"abc"
  ustr1b(2) = 4_"def"
  ustr1b(3) = 4_"gjh"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b(:)[1] = ustr1b(:)[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"def    " &
        .or. str2b(3) /= 1_"gjh    ") STOP 129
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 130
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1b(1) = 1_"abc"
  str1b(2) = 1_"def"
  str1b(3) = 1_"gjh"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b(:)[1] = str1b(:)[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"def    " &
        .or. ustr2b(3) /= 4_"gjh    ") STOP 131
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 132
  end if

  ! contiguous ARRAY - kind 1 <- 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2b(1) = 4_"abcdefg"
  ustr2b(2) = 4_"hijklmn"
  ustr2b(3) = 4_"opqrstu"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b(:)[1] = ustr2b(:)[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"hij" &
        .or. str1b(3) /= 1_"opq") STOP 133
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 134
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2b(1) = 1_"abcdefg"
  str2b(2) = 1_"hijklmn"
  str2b(3) = 1_"opqrstu"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b(:)[1] = str2b(:)[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"hij" &
        .or. ustr1b(3) /= 4_"opq") STOP 135
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 136
  end if

  ! - - - - - array = scalar

  ! contiguous ARRAY - kind 1 <- 4 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr1a = 4_"abc"
  str2b(1) = 1_"XXXXXXX"
  str2b(2) = 1_"YYYYYYY"
  str2b(3) = 1_"ZZZZZZZ"
  if (this_image() == num_images()) then
    str2b(:)[1] = ustr1a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str2b(1) /= 1_"abc    " .or. str2b(2) /= 1_"abc    " &
        .or. str2b(3) /= 1_"abc    ") STOP 137
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") STOP 138
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with padding
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str1a = 1_"abc"
  ustr2b(1) = 4_"XXXXXXX"
  ustr2b(2) = 4_"YYYYYYY"
  ustr2b(3) = 4_"ZZZZZZZ"
  if (this_image() == num_images()) then
    ustr2b(:)[1] = str1a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr2b(1) /= 4_"abc    " .or. ustr2b(2) /= 4_"abc    " &
        .or. ustr2b(3) /= 4_"abc    ") STOP 139
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") STOP 140
  end if

  ! contiguous ARRAY - kind 1 <- 4 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  ustr2a = 4_"abcdefg"
  str1b(1) = 1_"XXX"
  str1b(2) = 1_"YYY"
  str1b(3) = 1_"ZZZ"
  if (this_image() == num_images()) then
    str1b(:)[1] = ustr2a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (str1b(1) /= 1_"abc" .or. str1b(2) /= 1_"abc" &
        .or. str1b(3) /= 1_"abc") STOP 141
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") STOP 142
  end if

  ! contiguous ARRAY - kind 4 <- 1 - with trimming
  str1a = 1_"zzz"; str1b = 1_"zzz"; ustr1a = 4_"zzz"; ustr1b = 4_"zzz"
  str2a = 1_"zzzzzzzz"; str2b = 1_"zzzzzzzz"
  ustr2a = 4_"zzzzzzzz"; ustr2b = 4_"zzzzzzzz"
  str2a = 1_"abcdefg"
  ustr1b(1) = 4_"XXX"
  ustr1b(2) = 4_"YYY"
  ustr1b(3) = 4_"ZZZ"
  if (this_image() == num_images()) then
    ustr1b(:)[1] = str2a[mod(1, num_images())+1]
  end if
  sync all
  if (this_image() == 1) then
    if (ustr1b(1) /= 4_"abc" .or. ustr1b(2) /= 4_"abc" &
        .or. ustr1b(3) /= 4_"abc") STOP 143
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") STOP 144
  end if

end subroutine char_test
end program test
