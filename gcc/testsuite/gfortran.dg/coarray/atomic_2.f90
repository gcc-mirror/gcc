! { dg-do run }
!
use iso_fortran_env, only: atomic_int_kind, atomic_logical_kind
implicit none

intrinsic :: atomic_define
intrinsic :: atomic_ref
intrinsic :: atomic_cas
intrinsic :: atomic_add
intrinsic :: atomic_and
intrinsic :: atomic_or
intrinsic :: atomic_xor
intrinsic :: atomic_fetch_add
intrinsic :: atomic_fetch_and
intrinsic :: atomic_fetch_or
intrinsic :: atomic_fetch_xor
integer(atomic_int_kind) :: caf[*], var, var3
logical(atomic_logical_kind) :: caf_log[*], var2
integer :: stat, i

caf = 0
caf_log = .false.
sync all

if (this_image() == 1) then
  call atomic_define(caf[num_images()], 5, stat=stat)
  if (stat /= 0) STOP 1
  call atomic_define(caf_log[num_images()], .true., stat=stat)
  if (stat /= 0) STOP 2
end if
sync all

if (this_image() == num_images()) then
  if (caf /= 5) STOP 3
  if (.not. caf_log) STOP 4
  var = 99
  call atomic_ref(var, caf, stat=stat)
  if (stat /= 0 .or. var /= 5) STOP 5
  var2 = .false.
  call atomic_ref(var2, caf_log, stat=stat)
  if (stat /= 0 .or. .not. var2) STOP 6
end if
call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= 5) STOP 7
call atomic_ref(var2, caf_log[num_images()], stat=stat)
if (stat /= 0 .or. .not. var2) STOP 8
sync all

! ADD
caf = 0
sync all

call atomic_add(caf, this_image(), stat=stat)
if (stat /= 0) STOP 9
do i = 1, num_images()
  call atomic_add(caf[i], 1, stat=stat)
  if (stat /= 0) STOP 10
  call atomic_ref(var, caf, stat=stat)
  if (stat /= 0 .or. var < this_image()) STOP 11
end do
sync all

call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= num_images() + this_image()) STOP 12
do i = 1, num_images()
  call atomic_ref(var, caf[i], stat=stat)
  if (stat /= 0 .or. var /= num_images() + i) STOP 13
end do
sync all

! AND(1)
caf = 0
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_and(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) STOP 14
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 15
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 16
    end if
  end do
end if
sync all

! AND(2)
caf = -1
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_and(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) STOP 17
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 18
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 19
    end if
  end do
end if
sync all

! AND(3)
caf = 0
do i = 1, storage_size(caf)-2, 2
  caf = shiftl(1, i)
  var3 = shiftl(1, i)
end do
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_and(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) STOP 20
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 21
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 22
    end if
  end do
end if
sync all

! OR(1)
caf = 0
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_or(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) STOP 23
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 24
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 25
    end if
  end do
end if
sync all

! OR(2)
caf = -1
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_or(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) STOP 26
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 27
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 28
    end if
  end do
end if
sync all

! OR(3)
caf = 0
do i = 1, storage_size(caf)-2, 2
  caf = shiftl(1, i)
  var3 = shiftl(1, i)
end do
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_or(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) STOP 29
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 30
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 31
    end if
  end do
end if
sync all

! XOR(1)
caf = 0
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_xor(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) STOP 32
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 33
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 34
    end if
  end do
end if
sync all

! XOR(2)
caf = -1
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_xor(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) STOP 35
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 36
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 37
    end if
  end do
end if
sync all

! XOR(3)
caf = 0
do i = 1, storage_size(caf)-2, 2
  caf = shiftl(1, i)
  var3 = shiftl(1, i)
end do
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_xor(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) STOP 38
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 39
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 40
    end if
  end do
end if
sync all

! ADD
caf = 0
sync all
var = -99
call atomic_fetch_add(caf, this_image(), var, stat=stat)
if (stat /= 0 .or. var < 0) STOP 41
if (num_images() == 1 .and. var /= 0) STOP 42
do i = 1, num_images()
  var = -99
  call atomic_fetch_add(caf[i], 1, var, stat=stat)
  if (stat /= 0 .or. var < 0) STOP 43
  call atomic_ref(var, caf, stat=stat)
  if (stat /= 0 .or. var < this_image()) STOP 44
end do
sync all

call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= num_images() + this_image()) STOP 45
do i = 1, num_images()
  call atomic_ref(var, caf[i], stat=stat)
  if (stat /= 0 .or. var /= num_images() + i) STOP 46
end do
sync all


! AND(1)
caf = 0
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = 99
    call atomic_fetch_and(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. var /= 0) STOP 47
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 48
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 49
    end if
  end do
end if
sync all

! AND(2)
caf = -1
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = -99
    call atomic_fetch_and(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. var == shiftl(1, this_image())) STOP 50
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 51
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 52
    end if
  end do
end if
sync all

! AND(3)
caf = 0
var3 = 0
do i = 1, storage_size(caf)-2, 2
  caf = ior(shiftl(1, i), caf)
  var3 = ior(shiftl(1, i), var3)
end do
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = -99
    call atomic_fetch_and(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. var <= 0) STOP 53
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 54
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 55
    end if
  end do
end if
sync all



! OR(1)
caf = 0
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = -99
    call atomic_fetch_or(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. var < 0 .or. var == shiftl(1, this_image())) STOP 56
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 57
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 58
    end if
  end do
end if
sync all

! OR(2)
caf = -1
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = -99
    call atomic_fetch_or(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. (var < 0 .and. var /= -1)) STOP 59
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 60
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 61
    end if
  end do
end if
sync all

! OR(3)
caf = 0
var3 = 0
do i = 1, storage_size(caf)-2, 2
  caf = ior(shiftl(1, i), caf)
  var3 = ior(shiftl(1, i), var3)
end do
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = -99
    call atomic_fetch_or(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. var <= 0) STOP 62
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 63
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 64
    end if
  end do
end if
sync all


! XOR(1)
caf = 0
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = -99
    call atomic_fetch_xor(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. var < 0 .or. var == shiftl(1, this_image())) STOP 65
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 66
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 67
    end if
  end do
end if
sync all

! XOR(2)
caf = -1
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = -99
    call atomic_fetch_xor(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. (var < 0 .and. var /= -1)) STOP 68
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 69
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 70
    end if
  end do
end if
sync all

! XOR(3)
caf = 0
var3 = 0
do i = 1, storage_size(caf)-2, 2
  caf = ior(shiftl(1, i), caf)
  var3 = ior(shiftl(1, i), var3)
end do
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = -99
    call atomic_fetch_xor(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. var <= 0) STOP 71
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) STOP 72
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) STOP 73
    end if
  end do
end if
sync all

! CAS
caf = 9
caf_log = .true.
sync all

if (this_image() == 1) then
  call atomic_cas(caf[num_images()], compare=5, new=3, old=var, stat=stat)
  if (stat /= 0 .or. var /= 9) STOP 74
  call atomic_ref(var, caf[num_images()], stat=stat)
  if (stat /= 0 .or. var /= 9) STOP 75
end if
sync all

if (this_image() == num_images() .and. caf /= 9) STOP 76
call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= 9) STOP 77
sync all

if (this_image() == 1) then
  call atomic_cas(caf[num_images()], compare=9, new=3, old=var, stat=stat)
  if (stat /= 0 .or. var /= 9) STOP 78
  call atomic_ref(var, caf[num_images()], stat=stat)
  if (stat /= 0 .or. var /= 3) STOP 79
end if
sync all

if (this_image() == num_images() .and. caf /= 3) STOP 80
call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= 3) STOP 81
sync all


if (this_image() == 1) then
  call atomic_cas(caf_log[num_images()], compare=.false., new=.false., old=var2, stat=stat)
  if (stat /= 0 .or. var2 .neqv. .true.) STOP 82
  call atomic_ref(var2, caf_log[num_images()], stat=stat)
  if (stat /= 0 .or. var2 .neqv. .true.) STOP 83
end if
sync all

if (this_image() == num_images() .and. caf_log .neqv. .true.) STOP 84
call atomic_ref(var2, caf_log[num_images()], stat=stat)
if (stat /= 0 .or. var2 .neqv. .true.) STOP 85
sync all

if (this_image() == 1) then
  call atomic_cas(caf_log[num_images()], compare=.true., new=.false., old=var2, stat=stat)
  if (stat /= 0 .or. var2 .neqv. .true.) STOP 86
  call atomic_ref(var2, caf_log[num_images()], stat=stat)
  if (stat /= 0 .or. var2 .neqv. .false.) STOP 87
end if
sync all

if (this_image() == num_images() .and. caf_log .neqv. .false.) STOP 88
call atomic_ref(var2, caf_log[num_images()], stat=stat)
if (stat /= 0 .or. var2 .neqv. .false.) STOP 89
end
