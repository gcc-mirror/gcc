/* PR c/111309 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 512
__attribute__((noipa)) int
clz512 (unsigned _BitInt(512) x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzd512 (unsigned _BitInt(512) x)
{
  return __builtin_clzg (x, __builtin_popcountg ((typeof (x)) ~(typeof (x)) 0));
}

__attribute__((noipa)) int
clzD512 (unsigned _BitInt(512) x, int y)
{
  return __builtin_clzg (x, y);
}

__attribute__((noipa)) int
ctz512 (unsigned _BitInt(512) x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzd512 (unsigned _BitInt(512) x)
{
  return __builtin_ctzg (x, __builtin_popcountg ((typeof (x)) ~(typeof (x)) 0));
}

__attribute__((noipa)) int
ctzD512 (unsigned _BitInt(512) x, int y)
{
  return __builtin_ctzg (x, y);
}

__attribute__((noipa)) int
clrsb512 (_BitInt(512) x)
{
  return __builtin_clrsbg (x);
}

__attribute__((noipa)) int
ffs512 (_BitInt(512) x)
{
  return __builtin_ffsg (x);
}

__attribute__((noipa)) int
parity512 (unsigned _BitInt(512) x)
{
  return __builtin_parityg (x);
}

__attribute__((noipa)) int
popcount512 (unsigned _BitInt(512) x)
{
  return __builtin_popcountg (x);
}
#endif

#if __BITINT_MAXWIDTH__ >= 523
__attribute__((noipa)) int
clz523 (unsigned _BitInt(523) x)
{
  return __builtin_clzg (x);
}

__attribute__((noipa)) int
clzd523 (unsigned _BitInt(523) x)
{
  return __builtin_clzg (x, __builtin_popcountg ((typeof (x)) ~(typeof (x)) 0));
}

__attribute__((noipa)) int
clzD523 (unsigned _BitInt(523) x, int y)
{
  return __builtin_clzg (x, y);
}

__attribute__((noipa)) int
ctz523 (unsigned _BitInt(523) x)
{
  return __builtin_ctzg (x);
}

__attribute__((noipa)) int
ctzd523 (unsigned _BitInt(523) x)
{
  return __builtin_ctzg (x, __builtin_popcountg ((typeof (x)) ~(typeof (x)) 0));
}

__attribute__((noipa)) int
ctzD523 (unsigned _BitInt(523) x, int y)
{
  return __builtin_ctzg (x, y);
}

__attribute__((noipa)) int
clrsb523 (_BitInt(523) x)
{
  return __builtin_clrsbg (x);
}

__attribute__((noipa)) int
ffs523 (_BitInt(523) x)
{
  return __builtin_ffsg (x);
}

__attribute__((noipa)) int
parity523 (unsigned _BitInt(523) x)
{
  return __builtin_parityg (x);
}

__attribute__((noipa)) int
popcount523 (unsigned _BitInt(523) x)
{
  return __builtin_popcountg (x);
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 512
  if (clzd512 (0) != 512
      || clzD512 (0, -1) != -1
      || ctzd512 (0) != 512
      || ctzD512 (0, 42) != 42
      || clrsb512 (0) != 512 - 1
      || ffs512 (0) != 0
      || parity512 (0) != 0
      || popcount512 (0) != 0
      || __builtin_clzg ((unsigned _BitInt(512)) 0, 512 + 32) != 512 + 32
      || __builtin_ctzg ((unsigned _BitInt(512)) 0, 512) != 512
      || __builtin_clrsbg ((_BitInt(512)) 0) != 512 - 1
      || __builtin_ffsg ((_BitInt(512)) 0) != 0
      || __builtin_parityg ((unsigned _BitInt(512)) 0) != 0
      || __builtin_popcountg ((unsigned _BitInt(512)) 0) != 0)
    __builtin_abort ();
  if (clz512 (-1) != 0
      || clzd512 (-1) != 0
      || clzD512 (-1, 0) != 0
      || ctz512 (-1) != 0
      || ctzd512 (-1) != 0
      || ctzD512 (-1, 17) != 0
      || clrsb512 (-1) != 512 - 1
      || ffs512 (-1) != 1
      || parity512 (-1) != 0
      || popcount512 (-1) != 512
      || __builtin_clzg ((unsigned _BitInt(512)) -1) != 0
      || __builtin_clzg ((unsigned _BitInt(512)) -1, 512 + 32) != 0
      || __builtin_ctzg ((unsigned _BitInt(512)) -1) != 0
      || __builtin_ctzg ((unsigned _BitInt(512)) -1, 512) != 0
      || __builtin_clrsbg ((_BitInt(512)) -1) != 512 - 1
      || __builtin_ffsg ((_BitInt(512)) -1) != 1
      || __builtin_parityg ((unsigned _BitInt(512)) -1) != 0
      || __builtin_popcountg ((unsigned _BitInt(512)) -1) != 512)
    __builtin_abort ();
  if (clz512 (((unsigned _BitInt(512)) -1) >> 24) != 24
      || clz512 (((unsigned _BitInt(512)) -1) >> 79) != 79
      || clz512 (1) != 512 - 1
      || clzd512 (((unsigned _BitInt(512)) -1) >> 139) != 139
      || clzd512 (2) != 512 - 2
      || ctz512 (((unsigned _BitInt(512)) -1) << 42) != 42
      || ctz512 (((unsigned _BitInt(512)) -1) << 57) != 57
      || ctz512 (0x4000000000000000000000uwb) != 86
      || ctzd512 (((unsigned _BitInt(512)) -1) << 149) != 149
      || ctzd512 (2) != 1
      || clrsb512 ((unsigned _BitInt(512 - 4)) -1) != 3
      || clrsb512 ((unsigned _BitInt(512 - 28)) -1) != 27
      || clrsb512 ((unsigned _BitInt(512 - 29)) -1) != 28
      || clrsb512 (~(unsigned _BitInt(512)) (unsigned _BitInt(512 - 68)) -1) != 67
      || clrsb512 (~(unsigned _BitInt(512)) (unsigned _BitInt(512 - 92)) -1) != 91
      || clrsb512 (~(unsigned _BitInt(512)) (unsigned _BitInt(512 - 93)) -1) != 92
      || ffs512 (((unsigned _BitInt(512)) -1) << 42) != 43
      || ffs512 (((unsigned _BitInt(512)) -1) << 57) != 58
      || ffs512 (0x4000000000000000000000uwb) != 87
      || ffs512 (((unsigned _BitInt(512)) -1) << 149) != 150
      || ffs512 (2) != 2
      || __builtin_clzg (((unsigned _BitInt(512)) -1) >> 24) != 24
      || __builtin_clzg (((unsigned _BitInt(512)) -1) >> 79) != 79
      || __builtin_clzg ((unsigned _BitInt(512)) 1) != 512 - 1
      || __builtin_clzg (((unsigned _BitInt(512)) -1) >> 139, 512) != 139
      || __builtin_clzg ((unsigned _BitInt(512)) 2, 512) != 512 - 2
      || __builtin_ctzg (((unsigned _BitInt(512)) -1) << 42) != 42
      || __builtin_ctzg (((unsigned _BitInt(512)) -1) << 57) != 57
      || __builtin_ctzg ((unsigned _BitInt(512)) 0x4000000000000000000000uwb) != 86
      || __builtin_ctzg (((unsigned _BitInt(512)) -1) << 149, 512) != 149
      || __builtin_ctzg ((unsigned _BitInt(512)) 2, 512) != 1
      || __builtin_clrsbg ((_BitInt(512)) (unsigned _BitInt(512 - 4)) -1) != 3
      || __builtin_clrsbg ((_BitInt(512)) (unsigned _BitInt(512 - 28)) -1) != 27
      || __builtin_clrsbg ((_BitInt(512)) (unsigned _BitInt(512 - 29)) -1) != 28
      || __builtin_clrsbg ((_BitInt(512)) ~(unsigned _BitInt(512)) (unsigned _BitInt(512 - 68)) -1) != 67
      || __builtin_clrsbg ((_BitInt(512)) ~(unsigned _BitInt(512)) (unsigned _BitInt(512 - 92)) -1) != 91
      || __builtin_clrsbg ((_BitInt(512)) ~(unsigned _BitInt(512)) (unsigned _BitInt(512 - 93)) -1) != 92
      || __builtin_ffsg ((_BitInt(512)) (((unsigned _BitInt(512)) -1) << 42)) != 43
      || __builtin_ffsg ((_BitInt(512)) (((unsigned _BitInt(512)) -1) << 57)) != 58
      || __builtin_ffsg ((_BitInt(512)) 0x4000000000000000000000uwb) != 87
      || __builtin_ffsg ((_BitInt(512)) (((unsigned _BitInt(512)) -1) << 149)) != 150
      || __builtin_ffsg ((_BitInt(512)) 2) != 2)
    __builtin_abort ();
  if (parity512 (8278593062772967967574644592392030907507244457324713380127157444008480135136016412791369421272159911061801023217823646324038055629840240503699995274750141uwb) != __builtin_parityg (8278593062772967967574644592392030907507244457324713380127157444008480135136016412791369421272159911061801023217823646324038055629840240503699995274750141uwb)
      || parity512 (663951521760319802637316646127146913163123967584512032007606686578544864655291546789196279408181546344880831465704154822174055168766759305688225967189384uwb) != __builtin_parityg (663951521760319802637316646127146913163123967584512032007606686578544864655291546789196279408181546344880831465704154822174055168766759305688225967189384uwb)
      || parity512 (8114152627481936575035564712656624361256533214211179387274127464949371919139038942819974113641465089580051998523156404968195970853124179018281296621919217uwb) != __builtin_parityg (8114152627481936575035564712656624361256533214211179387274127464949371919139038942819974113641465089580051998523156404968195970853124179018281296621919217uwb)
      || popcount512 (697171368046392901434470580443928282938585745214587494987284546386421344865289735592202298494880955572094546861862007016154025065165834164941207378563932uwb) != __builtin_popcountg (697171368046392901434470580443928282938585745214587494987284546386421344865289735592202298494880955572094546861862007016154025065165834164941207378563932uwb)
      || popcount512 (12625357869391866487124235043239209385173615631331705015179232007319637649427586947822360147798041278948617160703315666047585702906648747835331939389354450uwb) != __builtin_popcountg (12625357869391866487124235043239209385173615631331705015179232007319637649427586947822360147798041278948617160703315666047585702906648747835331939389354450uwb)
      || popcount512 (12989863959706456104163426941303698078341934896544520782734564901708926112239778316241786242633862403309192697330635825122310265805838908726925342761646021uwb) != __builtin_popcountg (12989863959706456104163426941303698078341934896544520782734564901708926112239778316241786242633862403309192697330635825122310265805838908726925342761646021uwb))
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 523
  if (clzd523 (0) != 523
      || clzD523 (0, 42) != 42
      || ctzd523 (0) != 523
      || ctzD523 (0, -1) != -1
      || clrsb523 (0) != 523 - 1
      || ffs523 (0) != 0
      || parity523 (0) != 0
      || popcount523 (0) != 0
      || __builtin_clzg ((unsigned _BitInt(523)) 0, 523 + 32) != 523 + 32
      || __builtin_ctzg ((unsigned _BitInt(523)) 0, 523) != 523
      || __builtin_clrsbg ((_BitInt(523)) 0) != 523 - 1
      || __builtin_ffsg ((_BitInt(523)) 0) != 0
      || __builtin_parityg ((unsigned _BitInt(523)) 0) != 0
      || __builtin_popcountg ((unsigned _BitInt(523)) 0) != 0)
    __builtin_abort ();
  if (clz523 (-1) != 0
      || clzd523 (-1) != 0
      || clzD523 (-1, 15) != 0
      || ctz523 (-1) != 0
      || ctzd523 (-1) != 0
      || ctzD523 (-1, -57) != 0
      || clrsb523 (-1) != 523 - 1
      || ffs523 (-1) != 1
      || parity523 (-1) != 1
      || popcount523 (-1) != 523
      || __builtin_clzg ((unsigned _BitInt(523)) -1) != 0
      || __builtin_clzg ((unsigned _BitInt(523)) -1, 523 + 32) != 0
      || __builtin_ctzg ((unsigned _BitInt(523)) -1) != 0
      || __builtin_ctzg ((unsigned _BitInt(523)) -1, 523) != 0
      || __builtin_clrsbg ((_BitInt(523)) -1) != 523 - 1
      || __builtin_ffsg ((_BitInt(523)) -1) != 1
      || __builtin_parityg ((unsigned _BitInt(523)) -1) != 1
      || __builtin_popcountg ((unsigned _BitInt(523)) -1) != 523)
    __builtin_abort ();
  if (clz523 (((unsigned _BitInt(523)) -1) >> 24) != 24
      || clz523 (((unsigned _BitInt(523)) -1) >> 79) != 79
      || clz523 (1) != 523 - 1
      || clzd523 (((unsigned _BitInt(523)) -1) >> 139) != 139
      || clzd523 (2) != 523 - 2
      || ctz523 (((unsigned _BitInt(523)) -1) << 42) != 42
      || ctz523 (((unsigned _BitInt(523)) -1) << 57) != 57
      || ctz523 (0x4000000000000000000000uwb) != 86
      || ctzd523 (((unsigned _BitInt(523)) -1) << 149) != 149
      || ctzd523 (2) != 1
      || clrsb523 ((unsigned _BitInt(523 - 4)) -1) != 3
      || clrsb523 ((unsigned _BitInt(523 - 28)) -1) != 27
      || clrsb523 ((unsigned _BitInt(523 - 29)) -1) != 28
      || clrsb523 (~(unsigned _BitInt(523)) (unsigned _BitInt(523 - 68)) -1) != 67
      || clrsb523 (~(unsigned _BitInt(523)) (unsigned _BitInt(523 - 92)) -1) != 91
      || clrsb523 (~(unsigned _BitInt(523)) (unsigned _BitInt(523 - 93)) -1) != 92
      || ffs523 (((unsigned _BitInt(523)) -1) << 42) != 43
      || ffs523 (((unsigned _BitInt(523)) -1) << 57) != 58
      || ffs523 (0x4000000000000000000000uwb) != 87
      || ffs523 (((unsigned _BitInt(523)) -1) << 149) != 150
      || ffs523 (2) != 2
      || __builtin_clzg (((unsigned _BitInt(523)) -1) >> 24) != 24
      || __builtin_clzg (((unsigned _BitInt(523)) -1) >> 79) != 79
      || __builtin_clzg ((unsigned _BitInt(523)) 1) != 523 - 1
      || __builtin_clzg (((unsigned _BitInt(523)) -1) >> 139, 523) != 139
      || __builtin_clzg ((unsigned _BitInt(523)) 2, 523) != 523 - 2
      || __builtin_ctzg (((unsigned _BitInt(523)) -1) << 42) != 42
      || __builtin_ctzg (((unsigned _BitInt(523)) -1) << 57) != 57
      || __builtin_ctzg ((unsigned _BitInt(523)) 0x4000000000000000000000uwb) != 86
      || __builtin_ctzg (((unsigned _BitInt(523)) -1) << 149, 523) != 149
      || __builtin_ctzg ((unsigned _BitInt(523)) 2, 523) != 1
      || __builtin_clrsbg ((_BitInt(523)) (unsigned _BitInt(523 - 4)) -1) != 3
      || __builtin_clrsbg ((_BitInt(523)) (unsigned _BitInt(523 - 28)) -1) != 27
      || __builtin_clrsbg ((_BitInt(523)) (unsigned _BitInt(523 - 29)) -1) != 28
      || __builtin_clrsbg ((_BitInt(523)) ~(unsigned _BitInt(523)) (unsigned _BitInt(523 - 68)) -1) != 67
      || __builtin_clrsbg ((_BitInt(523)) ~(unsigned _BitInt(523)) (unsigned _BitInt(523 - 92)) -1) != 91
      || __builtin_clrsbg ((_BitInt(523)) ~(unsigned _BitInt(523)) (unsigned _BitInt(523 - 93)) -1) != 92
      || __builtin_ffsg ((_BitInt(523)) (((unsigned _BitInt(523)) -1) << 42)) != 43
      || __builtin_ffsg ((_BitInt(523)) (((unsigned _BitInt(523)) -1) << 57)) != 58
      || __builtin_ffsg ((_BitInt(523)) 0x4000000000000000000000uwb) != 87
      || __builtin_ffsg ((_BitInt(523)) (((unsigned _BitInt(523)) -1) << 149)) != 150
      || __builtin_ffsg ((_BitInt(523)) 2) != 2)
    __builtin_abort ();
  if (parity523 (14226628251091586975416900831427560438504550751597528218770815297642064445318137709184907300499591292677456563377096100346699421879373024906380724757049700104uwb) != __builtin_parityg (14226628251091586975416900831427560438504550751597528218770815297642064445318137709184907300499591292677456563377096100346699421879373024906380724757049700104uwb)
      || parity523 (20688958227123188226117538663818621034852702121556301239818743230005799574164516085541310491875153692467123662601853835357822935286851364843928714141587045255uwb) != __builtin_parityg (20688958227123188226117538663818621034852702121556301239818743230005799574164516085541310491875153692467123662601853835357822935286851364843928714141587045255uwb)
      || parity523 (8927708174664018648856542263215989788443763271738485875573765922613438023117960552135374015673598803453205044464280019640319125968982118836809392169156450404uwb) != __builtin_parityg (8927708174664018648856542263215989788443763271738485875573765922613438023117960552135374015673598803453205044464280019640319125968982118836809392169156450404uwb)
      || popcount523 (27178327344587654457581274852432957423537947348354896748701960885269035920194935311522194372418922852798513401240689173265979378157685169921449935364246334672uwb) != __builtin_popcountg (27178327344587654457581274852432957423537947348354896748701960885269035920194935311522194372418922852798513401240689173265979378157685169921449935364246334672uwb)
      || popcount523 (5307736750284212829931201546806718535860789684371772688568780952567669490917265125893664418036905110148872995350655890585853451175740907670080602411287166989uwb) != __builtin_popcountg (5307736750284212829931201546806718535860789684371772688568780952567669490917265125893664418036905110148872995350655890585853451175740907670080602411287166989uwb)
      || popcount523 (21261096432069432668470452941790780841888331284195411465624030283325239673941548816191698556934198698768393659379577567450765073013688585051560340496749593370uwb) != __builtin_popcountg (21261096432069432668470452941790780841888331284195411465624030283325239673941548816191698556934198698768393659379577567450765073013688585051560340496749593370uwb))
    __builtin_abort ();
#endif
}
