! { dg-do run }
! { dg-options "-ff2c" }
! Verifies that complex pointer results work with -ff2c
! try all permutations of result clause in function yes/no
!                     and result clause in interface yes/no
! this is not possible in Fortran 77, but this exercises a previously
! buggy codepath
function c() result (r)
  common // z
  complex, pointer :: r
  complex, target :: z

  r=>z
end function c

function d()
  common // z
  complex, pointer :: d
  complex, target :: z

  d=>z
end function d

function e()
  common // z
  complex, pointer :: e
  complex, target :: z

  e=>z
end function e

function f() result(r)
  common // z
  complex, pointer :: r
  complex, target :: z

  r=>z
end function f

interface
   function c
     complex, pointer :: c
   end function c
end interface
interface
   function d
     complex, pointer :: d
   end function d
end interface
interface
   function e result(r)
     complex, pointer :: r
   end function e
end interface
interface
   function f result(r)
     complex, pointer :: r
   end function f
end interface

common // z
complex, target :: z
complex, pointer :: p

z = (1.,0.)
p => c()
z = (2.,0.)
if (p /= z) call abort ()

NULLIFY(p)
p => d()
z = (3.,0.)
if (p /= z) call abort ()

NULLIFY(p)
p => e()
z = (4.,0.)
if (p /= z) call abort ()

NULLIFY(p)
p => f()
z = (5.,0.)
if (p /= z) call abort ()
end
