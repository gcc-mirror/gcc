/* { dg-do compile } */
/* { dg-options "-fgimple -O2" } */

int __GIMPLE (ssa,guessed_local(12348030),startwith("fix_loops"))
un (int dd)
{
  int s2;
  int q8;
  int nz;

  __BB(2,guessed_local(12348030)):
  goto __BB3(guessed(134217728));

  __BB(3,loop_header(1),guessed_local(37044096)):
  nz_7 = __PHI (__BB2: 0, __BB5: nz_10);
  q8_13 = dd_9(D) * dd_9(D);
  q8_17 = q8_13 * q8_13;
  q8_21 = q8_17 * q8_17;
  q8_25 = q8_21 * q8_21;
  q8_29 = q8_25 * q8_25;
  q8_33 = q8_29 * q8_29;
  q8_37 = q8_33 * q8_33;
  q8_41 = q8_37 * q8_37;
  q8_45 = q8_41 * q8_41;
  q8_49 = q8_45 * q8_45;
  q8_53 = q8_49 * q8_49;
  q8_57 = q8_53 * q8_53;
  q8_61 = q8_57 * q8_57;
  q8_65 = q8_61 * q8_61;
  q8_69 = q8_65 * q8_65;
  q8_73 = q8_69 * q8_69;
  q8_77 = q8_73 * q8_73;
  q8_81 = q8_77 * q8_77;
  q8_85 = q8_81 * q8_81;
  q8_89 = q8_85 * q8_85;
  q8_93 = q8_89 * q8_89;
  q8_97 = q8_93 * q8_93;
  q8_101 = q8_97 * q8_97;
  q8_105 = q8_101 * q8_101;
  q8_109 = q8_105 * q8_105;
  q8_113 = q8_109 * q8_109;
  q8_117 = q8_113 * q8_113;
  q8_121 = q8_117 * q8_117;
  nz_10 = nz_7 + 1;
  if (nz_10 != 3)
    goto __BB5(guessed(89478485));
  else
    goto __BB4(guessed(44739243));

  __BB(5,guessed_local(24696064)):
  goto __BB3(precise(134217728));

  __BB(4,guessed_local(12348031)):
  return q8_121;

}
