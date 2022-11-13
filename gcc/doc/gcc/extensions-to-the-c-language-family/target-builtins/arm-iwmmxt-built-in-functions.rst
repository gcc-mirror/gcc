..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _arm-iwmmxt-built-in-functions:

ARM iWMMXt Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the ARM family of
processors when the :option:`-mcpu=iwmmxt` switch is used:

.. code-block:: c++

  typedef int v2si __attribute__ ((vector_size (8)));
  typedef short v4hi __attribute__ ((vector_size (8)));
  typedef char v8qi __attribute__ ((vector_size (8)));

  int __builtin_arm_getwcgr0 (void);
  void __builtin_arm_setwcgr0 (int);
  int __builtin_arm_getwcgr1 (void);
  void __builtin_arm_setwcgr1 (int);
  int __builtin_arm_getwcgr2 (void);
  void __builtin_arm_setwcgr2 (int);
  int __builtin_arm_getwcgr3 (void);
  void __builtin_arm_setwcgr3 (int);
  int __builtin_arm_textrmsb (v8qi, int);
  int __builtin_arm_textrmsh (v4hi, int);
  int __builtin_arm_textrmsw (v2si, int);
  int __builtin_arm_textrmub (v8qi, int);
  int __builtin_arm_textrmuh (v4hi, int);
  int __builtin_arm_textrmuw (v2si, int);
  v8qi __builtin_arm_tinsrb (v8qi, int, int);
  v4hi __builtin_arm_tinsrh (v4hi, int, int);
  v2si __builtin_arm_tinsrw (v2si, int, int);
  long long __builtin_arm_tmia (long long, int, int);
  long long __builtin_arm_tmiabb (long long, int, int);
  long long __builtin_arm_tmiabt (long long, int, int);
  long long __builtin_arm_tmiaph (long long, int, int);
  long long __builtin_arm_tmiatb (long long, int, int);
  long long __builtin_arm_tmiatt (long long, int, int);
  int __builtin_arm_tmovmskb (v8qi);
  int __builtin_arm_tmovmskh (v4hi);
  int __builtin_arm_tmovmskw (v2si);
  long long __builtin_arm_waccb (v8qi);
  long long __builtin_arm_wacch (v4hi);
  long long __builtin_arm_waccw (v2si);
  v8qi __builtin_arm_waddb (v8qi, v8qi);
  v8qi __builtin_arm_waddbss (v8qi, v8qi);
  v8qi __builtin_arm_waddbus (v8qi, v8qi);
  v4hi __builtin_arm_waddh (v4hi, v4hi);
  v4hi __builtin_arm_waddhss (v4hi, v4hi);
  v4hi __builtin_arm_waddhus (v4hi, v4hi);
  v2si __builtin_arm_waddw (v2si, v2si);
  v2si __builtin_arm_waddwss (v2si, v2si);
  v2si __builtin_arm_waddwus (v2si, v2si);
  v8qi __builtin_arm_walign (v8qi, v8qi, int);
  long long __builtin_arm_wand(long long, long long);
  long long __builtin_arm_wandn (long long, long long);
  v8qi __builtin_arm_wavg2b (v8qi, v8qi);
  v8qi __builtin_arm_wavg2br (v8qi, v8qi);
  v4hi __builtin_arm_wavg2h (v4hi, v4hi);
  v4hi __builtin_arm_wavg2hr (v4hi, v4hi);
  v8qi __builtin_arm_wcmpeqb (v8qi, v8qi);
  v4hi __builtin_arm_wcmpeqh (v4hi, v4hi);
  v2si __builtin_arm_wcmpeqw (v2si, v2si);
  v8qi __builtin_arm_wcmpgtsb (v8qi, v8qi);
  v4hi __builtin_arm_wcmpgtsh (v4hi, v4hi);
  v2si __builtin_arm_wcmpgtsw (v2si, v2si);
  v8qi __builtin_arm_wcmpgtub (v8qi, v8qi);
  v4hi __builtin_arm_wcmpgtuh (v4hi, v4hi);
  v2si __builtin_arm_wcmpgtuw (v2si, v2si);
  long long __builtin_arm_wmacs (long long, v4hi, v4hi);
  long long __builtin_arm_wmacsz (v4hi, v4hi);
  long long __builtin_arm_wmacu (long long, v4hi, v4hi);
  long long __builtin_arm_wmacuz (v4hi, v4hi);
  v4hi __builtin_arm_wmadds (v4hi, v4hi);
  v4hi __builtin_arm_wmaddu (v4hi, v4hi);
  v8qi __builtin_arm_wmaxsb (v8qi, v8qi);
  v4hi __builtin_arm_wmaxsh (v4hi, v4hi);
  v2si __builtin_arm_wmaxsw (v2si, v2si);
  v8qi __builtin_arm_wmaxub (v8qi, v8qi);
  v4hi __builtin_arm_wmaxuh (v4hi, v4hi);
  v2si __builtin_arm_wmaxuw (v2si, v2si);
  v8qi __builtin_arm_wminsb (v8qi, v8qi);
  v4hi __builtin_arm_wminsh (v4hi, v4hi);
  v2si __builtin_arm_wminsw (v2si, v2si);
  v8qi __builtin_arm_wminub (v8qi, v8qi);
  v4hi __builtin_arm_wminuh (v4hi, v4hi);
  v2si __builtin_arm_wminuw (v2si, v2si);
  v4hi __builtin_arm_wmulsm (v4hi, v4hi);
  v4hi __builtin_arm_wmulul (v4hi, v4hi);
  v4hi __builtin_arm_wmulum (v4hi, v4hi);
  long long __builtin_arm_wor (long long, long long);
  v2si __builtin_arm_wpackdss (long long, long long);
  v2si __builtin_arm_wpackdus (long long, long long);
  v8qi __builtin_arm_wpackhss (v4hi, v4hi);
  v8qi __builtin_arm_wpackhus (v4hi, v4hi);
  v4hi __builtin_arm_wpackwss (v2si, v2si);
  v4hi __builtin_arm_wpackwus (v2si, v2si);
  long long __builtin_arm_wrord (long long, long long);
  long long __builtin_arm_wrordi (long long, int);
  v4hi __builtin_arm_wrorh (v4hi, long long);
  v4hi __builtin_arm_wrorhi (v4hi, int);
  v2si __builtin_arm_wrorw (v2si, long long);
  v2si __builtin_arm_wrorwi (v2si, int);
  v2si __builtin_arm_wsadb (v2si, v8qi, v8qi);
  v2si __builtin_arm_wsadbz (v8qi, v8qi);
  v2si __builtin_arm_wsadh (v2si, v4hi, v4hi);
  v2si __builtin_arm_wsadhz (v4hi, v4hi);
  v4hi __builtin_arm_wshufh (v4hi, int);
  long long __builtin_arm_wslld (long long, long long);
  long long __builtin_arm_wslldi (long long, int);
  v4hi __builtin_arm_wsllh (v4hi, long long);
  v4hi __builtin_arm_wsllhi (v4hi, int);
  v2si __builtin_arm_wsllw (v2si, long long);
  v2si __builtin_arm_wsllwi (v2si, int);
  long long __builtin_arm_wsrad (long long, long long);
  long long __builtin_arm_wsradi (long long, int);
  v4hi __builtin_arm_wsrah (v4hi, long long);
  v4hi __builtin_arm_wsrahi (v4hi, int);
  v2si __builtin_arm_wsraw (v2si, long long);
  v2si __builtin_arm_wsrawi (v2si, int);
  long long __builtin_arm_wsrld (long long, long long);
  long long __builtin_arm_wsrldi (long long, int);
  v4hi __builtin_arm_wsrlh (v4hi, long long);
  v4hi __builtin_arm_wsrlhi (v4hi, int);
  v2si __builtin_arm_wsrlw (v2si, long long);
  v2si __builtin_arm_wsrlwi (v2si, int);
  v8qi __builtin_arm_wsubb (v8qi, v8qi);
  v8qi __builtin_arm_wsubbss (v8qi, v8qi);
  v8qi __builtin_arm_wsubbus (v8qi, v8qi);
  v4hi __builtin_arm_wsubh (v4hi, v4hi);
  v4hi __builtin_arm_wsubhss (v4hi, v4hi);
  v4hi __builtin_arm_wsubhus (v4hi, v4hi);
  v2si __builtin_arm_wsubw (v2si, v2si);
  v2si __builtin_arm_wsubwss (v2si, v2si);
  v2si __builtin_arm_wsubwus (v2si, v2si);
  v4hi __builtin_arm_wunpckehsb (v8qi);
  v2si __builtin_arm_wunpckehsh (v4hi);
  long long __builtin_arm_wunpckehsw (v2si);
  v4hi __builtin_arm_wunpckehub (v8qi);
  v2si __builtin_arm_wunpckehuh (v4hi);
  long long __builtin_arm_wunpckehuw (v2si);
  v4hi __builtin_arm_wunpckelsb (v8qi);
  v2si __builtin_arm_wunpckelsh (v4hi);
  long long __builtin_arm_wunpckelsw (v2si);
  v4hi __builtin_arm_wunpckelub (v8qi);
  v2si __builtin_arm_wunpckeluh (v4hi);
  long long __builtin_arm_wunpckeluw (v2si);
  v8qi __builtin_arm_wunpckihb (v8qi, v8qi);
  v4hi __builtin_arm_wunpckihh (v4hi, v4hi);
  v2si __builtin_arm_wunpckihw (v2si, v2si);
  v8qi __builtin_arm_wunpckilb (v8qi, v8qi);
  v4hi __builtin_arm_wunpckilh (v4hi, v4hi);
  v2si __builtin_arm_wunpckilw (v2si, v2si);
  long long __builtin_arm_wxor (long long, long long);
  long long __builtin_arm_wzero ();