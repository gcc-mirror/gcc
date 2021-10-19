! PR fortran/102745

implicit none

type t
end type t

type, extends(t) :: t2
end type t2

type t3
end type t3

type(t), allocatable :: var
type(t2), allocatable :: v2ar
type(t3), allocatable :: v3ar
class(t), allocatable :: cvar
class(t2), allocatable :: c2var
class(t3), allocatable :: c3var

call f(var)
call f(v2ar)   ! { dg-error "passed TYPE.t2. to TYPE.t." }
call f(v2ar%t)
call f(cvar)
call f(c2var)  ! { dg-error "passed CLASS.t2. to TYPE.t." }
call f(c2var%t)

call f2(var)   ! { dg-error "passed TYPE.t. to TYPE.t2." }
call f2(v2ar)
call f2(cvar)  ! { dg-error "passed CLASS.t. to TYPE.t2." }
call f2(c2var)


var = var
var = v2ar  ! { dg-error "TYPE.t2. to TYPE.t." }
var = cvar
var = c2var ! { dg-error "TYPE.t2. to TYPE.t." }

v2ar = var  ! { dg-error "Cannot convert TYPE.t. to TYPE.t2." }
v2ar = v2ar
v2ar = cvar ! { dg-error "Cannot convert TYPE.t. to TYPE.t2." }
v2ar = c2var

cvar = var
cvar = v2ar
cvar = cvar
cvar = c2var

c2var = var   ! { dg-error "Cannot convert TYPE.t. to CLASS.t2." }
c2var = v3ar  ! { dg-error "Cannot convert TYPE.t3. to CLASS.t2." }
c2var = v2ar
c2var = cvar  ! { dg-error "Cannot convert CLASS.t. to CLASS.t2." }
c2var = c3var ! { dg-error "Cannot convert CLASS.t3. to CLASS.t2." }
c2var = c2var

allocate (var, source=var)
allocate (var, source=v2ar)   ! { dg-error "incompatible with source-expr" }
allocate (var, source=cvar)
allocate (var, source=c2var)  ! { dg-error "incompatible with source-expr" }

allocate (v2ar, source=var)   ! { dg-error "incompatible with source-expr" }
allocate (v2ar, source=v2ar)
allocate (v2ar, source=cvar)  ! { dg-error "incompatible with source-expr" }
allocate (v2ar, source=c2var)

allocate (cvar, source=var)
allocate (cvar, source=v2ar)
allocate (cvar, source=cvar)
allocate (cvar, source=c2var)

allocate (c2var, source=var)  ! { dg-error "incompatible with source-expr" }
allocate (c2var, source=v2ar)
allocate (c2var, source=cvar) ! { dg-error "incompatible with source-expr" }
allocate (c2var, source=c2var)

contains
 subroutine f(x)
   type(t) :: x
 end
 subroutine f2(x)
   type(t2) :: x
 end
end
