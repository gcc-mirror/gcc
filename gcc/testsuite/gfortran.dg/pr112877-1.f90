! { dg-do compile }
! { dg-options "-Os" }

program test
    use iso_c_binding, only: c_short
    interface
      subroutine foo(a) bind(c)
        import c_short
        integer(kind=c_short), intent(in), value :: a
      end subroutine foo
    end interface
    integer(kind=c_short) a(5);
    call foo (a(3))
end

! { dg-final { scan-assembler "movswl\t10\\(%rsp\\), %edi" { target { { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* x86_64-*-gnu* } && { ! ia32 } } } } }
! { dg-final { scan-assembler "movswl\t-14\\(%ebp\\), %eax" { target { { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* x86_64-*-gnu* } && { ia32 } } } } }
