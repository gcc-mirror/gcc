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
  if (stat /= 0) call abort()
  call atomic_define(caf_log[num_images()], .true., stat=stat)
  if (stat /= 0) call abort()
end if
sync all

if (this_image() == num_images()) then
  if (caf /= 5) call abort()
  if (.not. caf_log) call abort()
  var = 99
  call atomic_ref(var, caf, stat=stat)
  if (stat /= 0 .or. var /= 5) call abort()
  var2 = .false.
  call atomic_ref(var2, caf_log, stat=stat)
  if (stat /= 0 .or. .not. var2) call abort()
end if
call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= 5) call abort()
call atomic_ref(var2, caf_log[num_images()], stat=stat)
if (stat /= 0 .or. .not. var2) call abort()
sync all

! ADD
caf = 0
sync all

call atomic_add(caf, this_image(), stat=stat)
if (stat /= 0) call abort()
do i = 1, num_images()
  call atomic_add(caf[i], 1, stat=stat)
  if (stat /= 0) call abort()
  call atomic_ref(var, caf, stat=stat)
  if (stat /= 0 .or. var < this_image()) call abort()
end do
sync all

call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= num_images() + this_image()) call abort()
do i = 1, num_images()
  call atomic_ref(var, caf[i], stat=stat)
  if (stat /= 0 .or. var /= num_images() + i) call abort()
end do
sync all

! AND(1)
caf = 0
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    call atomic_and(caf[i], shiftl(1, this_image()), stat=stat)
    if (stat /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
    end if
  end do
end if
sync all

! ADD
caf = 0
sync all
var = -99
call atomic_fetch_add(caf, this_image(), var, stat=stat)
if (stat /= 0 .or. var < 0) call abort()
if (num_images() == 1 .and. var /= 0) call abort()
do i = 1, num_images()
  var = -99
  call atomic_fetch_add(caf[i], 1, var, stat=stat)
  if (stat /= 0 .or. var < 0) call abort()
  call atomic_ref(var, caf, stat=stat)
  if (stat /= 0 .or. var < this_image()) call abort()
end do
sync all

call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= num_images() + this_image()) call abort()
do i = 1, num_images()
  call atomic_ref(var, caf[i], stat=stat)
  if (stat /= 0 .or. var /= num_images() + i) call abort()
end do
sync all


! AND(1)
caf = 0
sync all

if (this_image() < storage_size(caf)-2) then
  do i = this_image(), min(num_images(), storage_size(caf)-2)
    var = 99
    call atomic_fetch_and(caf[i], shiftl(1, this_image()), var, stat=stat)
    if (stat /= 0 .or. var /= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0 .or. var == shiftl(1, this_image())) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0 .or. var <= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = iand(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0 .or. var < 0 .or. var == shiftl(1, this_image())) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0 .or. (var < 0 .and. var /= -1)) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0 .or. var <= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ior(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0 .or. var < 0 .or. var == shiftl(1, this_image())) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = 0
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0 .or. (var < 0 .and. var /= -1)) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  var3 = -1
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
    if (stat /= 0 .or. var <= 0) call abort()
  end do
end if
sync all

if (this_image() < storage_size(caf)-2) then
  do i = 1, min(num_images(), storage_size(caf)-2)
    var3 = ieor(var3, shiftl(1, i))
    call atomic_ref(var, caf[i], stat=stat)
    if (stat /= 0 .or. var /= var3) call abort()
    if (i == this_image()) then
      call atomic_ref(var, caf[i], stat=stat)
      if (stat /= 0 .or. var /= var3) call abort()
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
  if (stat /= 0 .or. var /= 9) call abort()
  call atomic_ref(var, caf[num_images()], stat=stat)
  if (stat /= 0 .or. var /= 9) call abort()
end if
sync all

if (this_image() == num_images() .and. caf /= 9) call abort()
call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= 9) call abort()
sync all

if (this_image() == 1) then
  call atomic_cas(caf[num_images()], compare=9, new=3, old=var, stat=stat)
  if (stat /= 0 .or. var /= 9) call abort()
  call atomic_ref(var, caf[num_images()], stat=stat)
  if (stat /= 0 .or. var /= 3) call abort()
end if
sync all

if (this_image() == num_images() .and. caf /= 3) call abort()
call atomic_ref(var, caf[num_images()], stat=stat)
if (stat /= 0 .or. var /= 3) call abort()
sync all


if (this_image() == 1) then
  call atomic_cas(caf_log[num_images()], compare=.false., new=.false., old=var2, stat=stat)
  if (stat /= 0 .or. var2 .neqv. .true.) call abort()
  call atomic_ref(var2, caf_log[num_images()], stat=stat)
  if (stat /= 0 .or. var2 .neqv. .true.) call abort()
end if
sync all

if (this_image() == num_images() .and. caf_log .neqv. .true.) call abort()
call atomic_ref(var2, caf_log[num_images()], stat=stat)
if (stat /= 0 .or. var2 .neqv. .true.) call abort()
sync all

if (this_image() == 1) then
  call atomic_cas(caf_log[num_images()], compare=.true., new=.false., old=var2, stat=stat)
  if (stat /= 0 .or. var2 .neqv. .true.) call abort()
  call atomic_ref(var2, caf_log[num_images()], stat=stat)
  if (stat /= 0 .or. var2 .neqv. .false.) call abort()
end if
sync all

if (this_image() == num_images() .and. caf_log .neqv. .false.) call abort()
call atomic_ref(var2, caf_log[num_images()], stat=stat)
if (stat /= 0 .or. var2 .neqv. .false.) call abort()
end
