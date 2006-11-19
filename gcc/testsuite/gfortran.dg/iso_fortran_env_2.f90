! { dg-do compile }
module iso_fortran_env
  logical :: x
end module iso_fortran_env

subroutine bar1
  use , intrinsic :: iso_fortran_env
  print *, character_storage_size
end

subroutine bar2
  use, intrinsic :: iso_fortran_env
  print *, character_storage_size
end

subroutine bar3
  use,intrinsic :: iso_fortran_env
  print *, character_storage_size
end

subroutine bar4
  use,intrinsic::iso_fortran_env
  print *, character_storage_size
end

subroutine bar5
  use ,intrinsic :: iso_fortran_env
  print *, character_storage_size
end

subroutine foo1
  use :: iso_fortran_env
  print *, x
end

subroutine foo2
  use:: iso_fortran_env
  print *, x
end

subroutine foo3
  use::iso_fortran_env
  print *, x
end

subroutine foo4
  use  ::iso_fortran_env
  print *, x
end

subroutine gee1
  use , non_intrinsic :: iso_fortran_env
  print *, x
end

subroutine gee2
  use, non_intrinsic :: iso_fortran_env
  print *, x
end

subroutine gee3
  use,non_intrinsic :: iso_fortran_env
  print *, x
end

subroutine gee4
  use,non_intrinsic::iso_fortran_env
  print *, x
end

subroutine gee5
  use ,non_intrinsic :: iso_fortran_env
  print *, x
end

! { dg-final { cleanup-modules "iso_fortran_env" } }
