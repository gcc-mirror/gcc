/* { dg-do compile } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mcpu=*" } { "-mcpu=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-mabi=*" } { "-mabi=iwmmxt" } } */
/* { dg-skip-if "Test is specific to the iWMMXt" { arm*-*-* } { "-march=*" } { "-march=iwmmxt" } } */
/* { dg-skip-if "Test is specific to ARM mode" { arm*-*-* } { "-mthumb" } { "" } } */
/* { dg-require-effective-target arm32 } */
/* { dg-require-effective-target arm_iwmmxt_ok } */
/* { dg-options "-flax-vector-conversions -std=gnu99" } */

/* Internal data types for implementing the intrinsics.  */
typedef int __v2si __attribute__ ((vector_size (8)));
typedef short __v4hi __attribute__ ((vector_size (8)));
typedef signed char __v8qi __attribute__ ((vector_size (8)));

void
foo(void)
{
  volatile int isink;
  volatile long long llsink;
  volatile __v8qi v8sink;
  volatile __v4hi v4sink;
  volatile __v2si v2sink;

  isink = __builtin_arm_getwcgr0 ();
  __builtin_arm_setwcgr0 (isink);
  isink = __builtin_arm_getwcgr1 ();
  __builtin_arm_setwcgr1 (isink);
  isink = __builtin_arm_getwcgr2 ();
  __builtin_arm_setwcgr2 (isink);
  isink = __builtin_arm_getwcgr3 ();
  __builtin_arm_setwcgr3 (isink);

  isink = __builtin_arm_textrmsb (v8sink, 0);
  isink = __builtin_arm_textrmsh (v4sink, 0);
  isink = __builtin_arm_textrmsw (v2sink, 0);
  isink = __builtin_arm_textrmub (v8sink, 0);
  isink = __builtin_arm_textrmuh (v4sink, 0);
  isink = __builtin_arm_textrmuw (v2sink, 0);
  v8sink = __builtin_arm_tinsrb (v8sink, isink, 0);
  v4sink = __builtin_arm_tinsrh (v4sink, isink, 0);
  v2sink = __builtin_arm_tinsrw (v2sink, isink, 0);
  llsink = __builtin_arm_tmia (llsink, isink, isink);
  llsink = __builtin_arm_tmiabb (llsink, isink, isink);
  llsink = __builtin_arm_tmiabt (llsink, isink, isink);
  llsink = __builtin_arm_tmiaph (llsink, isink, isink);
  llsink = __builtin_arm_tmiatb (llsink, isink, isink);
  llsink = __builtin_arm_tmiatt (llsink, isink, isink);
  isink = __builtin_arm_tmovmskb (v8sink);
  isink = __builtin_arm_tmovmskh (v4sink);
  isink = __builtin_arm_tmovmskw (v2sink);
  llsink = __builtin_arm_waccb (v8sink);
  llsink = __builtin_arm_wacch (v4sink);
  llsink = __builtin_arm_waccw (v2sink);
  v8sink = __builtin_arm_waddb (v8sink, v8sink);
  v8sink = __builtin_arm_waddbss (v8sink, v8sink);
  v8sink = __builtin_arm_waddbus (v8sink, v8sink);
  v4sink = __builtin_arm_waddh (v4sink, v4sink);
  v4sink = __builtin_arm_waddhss (v4sink, v4sink);
  v4sink = __builtin_arm_waddhus (v4sink, v4sink);
  v2sink = __builtin_arm_waddw (v2sink, v2sink);
  v2sink = __builtin_arm_waddwss (v2sink, v2sink);
  v2sink = __builtin_arm_waddwus (v2sink, v2sink);
  v8sink = __builtin_arm_walign (v8sink, v8sink, 0);  /* waligni: 3-bit immediate.  */
  v8sink = __builtin_arm_walign (v8sink, v8sink, isink); /* walignr: GP register.  */
  llsink = __builtin_arm_wand(llsink, llsink);
  llsink = __builtin_arm_wandn (llsink, llsink);
  v8sink = __builtin_arm_wavg2b (v8sink, v8sink);
  v8sink = __builtin_arm_wavg2br (v8sink, v8sink);
  v4sink = __builtin_arm_wavg2h (v4sink, v4sink);
  v4sink = __builtin_arm_wavg2hr (v4sink, v4sink);
  v8sink = __builtin_arm_wcmpeqb (v8sink, v8sink);
  v4sink = __builtin_arm_wcmpeqh (v4sink, v4sink);
  v2sink = __builtin_arm_wcmpeqw (v2sink, v2sink);
  v8sink = __builtin_arm_wcmpgtsb (v8sink, v8sink);
  v4sink = __builtin_arm_wcmpgtsh (v4sink, v4sink);
  v2sink = __builtin_arm_wcmpgtsw (v2sink, v2sink);
  v8sink = __builtin_arm_wcmpgtub (v8sink, v8sink);
  v4sink = __builtin_arm_wcmpgtuh (v4sink, v4sink);
  v2sink = __builtin_arm_wcmpgtuw (v2sink, v2sink);
  llsink = __builtin_arm_wmacs (llsink, v4sink, v4sink);
  llsink = __builtin_arm_wmacsz (v4sink, v4sink);
  llsink = __builtin_arm_wmacu (llsink, v4sink, v4sink);
  llsink = __builtin_arm_wmacuz (v4sink, v4sink);
  v4sink = __builtin_arm_wmadds (v4sink, v4sink);
  v4sink = __builtin_arm_wmaddu (v4sink, v4sink);
  v8sink = __builtin_arm_wmaxsb (v8sink, v8sink);
  v4sink = __builtin_arm_wmaxsh (v4sink, v4sink);
  v2sink = __builtin_arm_wmaxsw (v2sink, v2sink);
  v8sink = __builtin_arm_wmaxub (v8sink, v8sink);
  v4sink = __builtin_arm_wmaxuh (v4sink, v4sink);
  v2sink = __builtin_arm_wmaxuw (v2sink, v2sink);
  v8sink = __builtin_arm_wminsb (v8sink, v8sink);
  v4sink = __builtin_arm_wminsh (v4sink, v4sink);
  v2sink = __builtin_arm_wminsw (v2sink, v2sink);
  v8sink = __builtin_arm_wminub (v8sink, v8sink);
  v4sink = __builtin_arm_wminuh (v4sink, v4sink);
  v2sink = __builtin_arm_wminuw (v2sink, v2sink);
  v4sink = __builtin_arm_wmulsm (v4sink, v4sink);
  v4sink = __builtin_arm_wmulul (v4sink, v4sink);
  v4sink = __builtin_arm_wmulum (v4sink, v4sink);
  llsink = __builtin_arm_wor (llsink, llsink);
  v2sink = __builtin_arm_wpackdss (llsink, llsink);
  v2sink = __builtin_arm_wpackdus (llsink, llsink);
  v8sink = __builtin_arm_wpackhss (v4sink, v4sink);
  v8sink = __builtin_arm_wpackhus (v4sink, v4sink);
  v4sink = __builtin_arm_wpackwss (v2sink, v2sink);
  v4sink = __builtin_arm_wpackwus (v2sink, v2sink);
  llsink = __builtin_arm_wrord (llsink, llsink);
  llsink = __builtin_arm_wrordi (llsink, isink);
  v4sink = __builtin_arm_wrorh (v4sink, llsink);
  v4sink = __builtin_arm_wrorhi (v4sink, isink);
  v2sink = __builtin_arm_wrorw (v2sink, llsink);
  v2sink = __builtin_arm_wrorwi (v2sink, isink);
  v2sink = __builtin_arm_wsadb (v2sink, v8sink, v8sink);
  v2sink = __builtin_arm_wsadbz (v8sink, v8sink);
  v2sink = __builtin_arm_wsadh (v2sink, v4sink, v4sink);
  v2sink = __builtin_arm_wsadhz (v4sink, v4sink);
  v4sink = __builtin_arm_wshufh (v4sink, 0);
  llsink = __builtin_arm_wslld (llsink, llsink);
  llsink = __builtin_arm_wslldi (llsink, 0);
  v4sink = __builtin_arm_wsllh (v4sink, llsink);
  v4sink = __builtin_arm_wsllhi (v4sink, isink);
  v2sink = __builtin_arm_wsllw (v2sink, llsink);
  v2sink = __builtin_arm_wsllwi (v2sink, isink);
  llsink = __builtin_arm_wsrad (llsink, llsink);
  llsink = __builtin_arm_wsradi (llsink, isink);
  v4sink = __builtin_arm_wsrah (v4sink, llsink);
  v4sink = __builtin_arm_wsrahi (v4sink, isink);
  v2sink = __builtin_arm_wsraw (v2sink, llsink);
  v2sink = __builtin_arm_wsrawi (v2sink, isink);
  llsink = __builtin_arm_wsrld (llsink, llsink);
  llsink = __builtin_arm_wsrldi (llsink, isink);
  v4sink = __builtin_arm_wsrlh (v4sink, llsink);
  v4sink = __builtin_arm_wsrlhi (v4sink, isink);
  v2sink = __builtin_arm_wsrlw (v2sink, llsink);
  v2sink = __builtin_arm_wsrlwi (v2sink, isink);
  v8sink = __builtin_arm_wsubb (v8sink, v8sink);
  v8sink = __builtin_arm_wsubbss (v8sink, v8sink);
  v8sink = __builtin_arm_wsubbus (v8sink, v8sink);
  v4sink = __builtin_arm_wsubh (v4sink, v4sink);
  v4sink = __builtin_arm_wsubhss (v4sink, v4sink);
  v4sink = __builtin_arm_wsubhus (v4sink, v4sink);
  v2sink = __builtin_arm_wsubw (v2sink, v2sink);
  v2sink = __builtin_arm_wsubwss (v2sink, v2sink);
  v2sink = __builtin_arm_wsubwus (v2sink, v2sink);
  v4sink = __builtin_arm_wunpckehsb (v8sink);
  v2sink = __builtin_arm_wunpckehsh (v4sink);
  llsink = __builtin_arm_wunpckehsw (v2sink);
  v4sink = __builtin_arm_wunpckehub (v8sink);
  v2sink = __builtin_arm_wunpckehuh (v4sink);
  llsink = __builtin_arm_wunpckehuw (v2sink);
  v4sink = __builtin_arm_wunpckelsb (v8sink);
  v2sink = __builtin_arm_wunpckelsh (v4sink);
  llsink = __builtin_arm_wunpckelsw (v2sink);
  v4sink = __builtin_arm_wunpckelub (v8sink);
  v2sink = __builtin_arm_wunpckeluh (v4sink);
  llsink = __builtin_arm_wunpckeluw (v2sink);
  v8sink = __builtin_arm_wunpckihb (v8sink, v8sink);
  v4sink = __builtin_arm_wunpckihh (v4sink, v4sink);
  v2sink = __builtin_arm_wunpckihw (v2sink, v2sink);
  v8sink = __builtin_arm_wunpckilb (v8sink, v8sink);
  v4sink = __builtin_arm_wunpckilh (v4sink, v4sink);
  v2sink = __builtin_arm_wunpckilw (v2sink, v2sink);
  llsink = __builtin_arm_wxor (llsink, llsink);
  llsink = __builtin_arm_wzero ();
}
