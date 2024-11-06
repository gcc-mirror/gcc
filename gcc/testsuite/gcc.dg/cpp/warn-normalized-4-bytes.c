// { dg-do preprocess }
// { dg-options "-std=gnu99 -Werror=normalized=nfc -fdiagnostics-show-caret -fdiagnostics-escape-format=bytes" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* གྷ = U+0F43 TIBETAN LETTER GHA, which has decomposition "0F42 0FB7" i.e.
   U+0F42 TIBETAN LETTER GA: ག
   U+0FB7 TIBETAN SUBJOINED LETTER HA: ྷ

   The UTF-8 encoding of U+0F43 TIBETAN LETTER GHA is: E0 BD 83.  */

foo before_\u0F43_after bar // { dg-error "'before_.U00000f43_after' is not in NFC .-Werror=normalized=." }
/* { dg-begin-multiline-output "" }
 foo before_\u0F43_after bar
     ^~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

foo before_གྷ_after bar // { dg-error "'before_.U00000f43_after' is not in NFC .-Werror=normalized=." }
/* { dg-begin-multiline-output "" }
 foo before_<e0><bd><83>_after bar
     ^~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
