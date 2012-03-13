
dnl Check if the target supports weak.
AC_DEFUN([GCC_CHECK_ATTRIBUTE_WEAK], [
  AC_CACHE_CHECK([whether the target supports weak],
		 ac_cv_have_attribute_weak, [
  weakref_m4_saved_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_TRY_COMPILE([void __attribute__((weak)) foo(void) { }],
		 [], ac_cv_have_attribute_weak=yes,
		 ac_cv_have_attribute_weak=no)
  CFLAGS="$weakref_m4_saved_CFLAGS"])
  if test x"$ac_cv_have_attribute_weak" = xyes; then
    AC_DEFINE(HAVE_ATTRIBUTE_WEAK, 1,
      [Define to 1 if the target supports __attribute__((weak)).])
  fi])

dnl Check whether weak refs work like the ELF ones.
dnl This means that the weak reference works without having to satify 
dnl linkage for the item.
dnl There are targets (at least Darwin) where we have fully functional
dnl weakrefs at runtime, but must supply the referenced item at link time.
AC_DEFUN([GCC_CHECK_ELF_STYLE_WEAKREF], [
  AC_CACHE_CHECK([whether weak refs work like ELF],
                  ac_cv_have_elf_style_weakref, [
  weakref_m4_saved_CFLAGS="$CFLAGS"
  case "${host}" in
    *-apple-darwin*) CFLAGS="$CFLAGS -Wl,-undefined,dynamic_lookup" ;;
    *) ;;
  esac  
  AC_RUN_IFELSE([AC_LANG_SOURCE([[
extern void fNotToBeFound(void) __attribute__((weak));
int main () 
{
  if (fNotToBeFound)
    return 1;
  else
    return 0;
}
]])], ac_cv_have_elf_style_weakref=yes, ac_cv_have_elf_style_weakref=no, [
case "${host}" in
  *-apple-darwin[[89]]*) ac_cv_have_elf_style_weakref=no ;;
  *) ac_cv_have_elf_style_weakref=yes;;
esac])CFLAGS="$weakref_m4_saved_CFLAGS"])
if test x"$ac_cv_have_elf_style_weakref" = xyes; then
  AC_DEFINE(HAVE_ELF_STYLE_WEAKREF, 1, [Define to 1 if target has a weakref that works like the ELF one.])
fi])

