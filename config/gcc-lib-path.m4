AC_DEFUN([TL_AC_GNU_MAKE_GCC_LIB_PATH],
[
if test x"$SET_GCC_LIB_PATH_CMD" != x; then
  # SET_GCC_LIB_PATH_CMD is "XXX=path; export XXX;". It is expanded to
  #
  #	eval "set_gcc_lib_path=XXX=path; export XXX;"
  #
  eval "set_gcc_lib_path=$SET_GCC_LIB_PATH_CMD"
  # It will set set_gcc_lib_path to "export XXX=path" for GNU make.
  set_gcc_lib_path="export $set_gcc_lib_path"
else
  set_gcc_lib_path=
fi
AC_SUBST(set_gcc_lib_path)
])dnl
