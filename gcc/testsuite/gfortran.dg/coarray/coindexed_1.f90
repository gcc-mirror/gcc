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
    if (str2a /= 1_"abc    ") call abort()
  else
    if (str2a /= 1_"XXXXXXX") call abort()
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
    if (ustr2a /= 4_"abc    ") call abort()
  else
    if (ustr2a /= 4_"XXXXXXX") call abort()
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
    if (str1a /= 1_"abc") call abort()
  else
    if (str1a /= 1_"XXX") call abort()
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
    if (ustr1a /= 4_"abc") call abort()
  else
    if (ustr1a /= 4_"XXX") call abort()
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
        .or. str2b(3) /= 1_"gjh    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"gjh    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"opq") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"opq") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
        .or. str2b(3) /= 1_"abc    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"abc    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"abc") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"abc") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
    if (str2a /= 1_"abc    ") call abort()
  else
    if (str2a /= 1_"XXXXXXX") call abort()
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
    if (ustr2a /= 4_"abc    ") call abort()
  else
    if (ustr2a /= 4_"XXXXXXX") call abort()
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
    if (str1a /= 1_"abc") call abort()
  else
    if (str1a /= 1_"XXX") call abort()
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
    if (ustr1a /= 4_"abc") call abort()
  else
    if (ustr1a /= 4_"XXX") call abort()
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
        .or. str2b(3) /= 1_"gjh    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"gjh    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"opq") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"opq") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
        .or. str2b(3) /= 1_"abc    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"abc    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"abc") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"abc") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
    if (str2a /= 1_"abc    ") call abort()
  else
    if (str2a /= 1_"XXXXXXX") call abort()
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
    if (ustr2a /= 4_"abc    ") call abort()
  else
    if (ustr2a /= 4_"XXXXXXX") call abort()
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
    if (str1a /= 1_"abc") call abort()
  else
    if (str1a /= 1_"XXX") call abort()
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
    if (ustr1a /= 4_"abc") call abort()
  else
    if (ustr1a /= 4_"XXX") call abort()
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
        .or. str2b(3) /= 1_"gjh    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"gjh    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"opq") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"opq") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
        .or. str2b(3) /= 1_"abc    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"abc    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"abc") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"abc") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
    if (str2a /= 1_"abc    ") call abort()
  else
    if (str2a /= 1_"XXXXXXX") call abort()
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
    if (ustr2a /= 4_"abc    ") call abort()
  else
    if (ustr2a /= 4_"XXXXXXX") call abort()
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
    if (str1a /= 1_"abc") call abort()
  else
    if (str1a /= 1_"XXX") call abort()
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
    if (ustr1a /= 4_"abc") call abort()
  else
    if (ustr1a /= 4_"XXX") call abort()
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
        .or. str2b(3) /= 1_"gjh    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"gjh    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"opq") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"opq") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
        .or. str2b(3) /= 1_"abc    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"abc    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"abc") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"abc") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
    if (str2a /= 1_"abc    ") call abort()
  else
    if (str2a /= 1_"XXXXXXX") call abort()
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
    if (ustr2a /= 4_"abc    ") call abort()
  else
    if (ustr2a /= 4_"XXXXXXX") call abort()
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
    if (str1a /= 1_"abc") call abort()
  else
    if (str1a /= 1_"XXX") call abort()
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
    if (ustr1a /= 4_"abc") call abort()
  else
    if (ustr1a /= 4_"XXX") call abort()
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
        .or. str2b(3) /= 1_"gjh    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"gjh    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"opq") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"opq") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
        .or. str2b(3) /= 1_"abc    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"abc    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"abc") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"abc") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
    if (str2a /= 1_"abc    ") call abort()
  else
    if (str2a /= 1_"XXXXXXX") call abort()
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
    if (ustr2a /= 4_"abc    ") call abort()
  else
    if (ustr2a /= 4_"XXXXXXX") call abort()
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
    if (str1a /= 1_"abc") call abort()
  else
    if (str1a /= 1_"XXX") call abort()
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
    if (ustr1a /= 4_"abc") call abort()
  else
    if (ustr1a /= 4_"XXX") call abort()
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
        .or. str2b(3) /= 1_"gjh    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"gjh    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"opq") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"opq") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
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
        .or. str2b(3) /= 1_"abc    ") call abort()
  else
    if (str2b(1) /= 1_"XXXXXXX" .or. str2b(2) /= 1_"YYYYYYY" &
        .or. str2b(3) /= 1_"ZZZZZZZ") call abort()
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
        .or. ustr2b(3) /= 4_"abc    ") call abort()
  else
    if (ustr2b(1) /= 4_"XXXXXXX" .or. ustr2b(2) /= 4_"YYYYYYY" &
        .or. ustr2b(3) /= 4_"ZZZZZZZ") call abort()
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
        .or. str1b(3) /= 1_"abc") call abort()
  else
    if (str1b(1) /= 1_"XXX" .or. str1b(2) /= 1_"YYY" &
        .or. str1b(3) /= 1_"ZZZ") call abort()
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
        .or. ustr1b(3) /= 4_"abc") call abort()
  else
    if (ustr1b(1) /= 4_"XXX" .or. ustr1b(2) /= 4_"YYY" &
        .or. ustr1b(3) /= 4_"ZZZ") call abort()
  end if

end subroutine char_test
end program test
