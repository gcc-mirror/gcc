! { dg-do compile }
!
! PR fortran/62044
! ICE when loading module UnstructuredGridImages
! because the type UnstructuredGridImageSiloForm
! is not accessible there under its name.
!
! Contributed by Reuben Budiardja <reubendb@gmail.com>

module UnstructuredGridImageSilo_Form
  implicit none
  private
  type, public, abstract :: GridImageSiloTemplate
  end type GridImageSiloTemplate
  type, public, extends ( GridImageSiloTemplate ) :: &
    UnstructuredGridImageSiloForm
  end type UnstructuredGridImageSiloForm
end module UnstructuredGridImageSilo_Form

module UnstructuredGridImages
  use UnstructuredGridImageSilo_Form, &
        UnstructuredGridImageForm => UnstructuredGridImageSiloForm
end module UnstructuredGridImages

module FileSystem
  use UnstructuredGridImages
end module FileSystem
