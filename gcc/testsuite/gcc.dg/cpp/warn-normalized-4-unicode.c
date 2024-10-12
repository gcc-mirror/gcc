// { dg-do preprocess }
// { dg-options "-std=gnu99 -Werror=normalized=nfc -fdiagnostics-show-caret -fdiagnostics-escape-format=unicode" }
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* གྷ = U+0F43 TIBETAN LETTER GHA, which has decomposition "0F42 0FB7" i.e.
   U+0F42 TIBETAN LETTER GA: ག
   U+0FB7 TIBETAN SUBJOINED LETTER HA: ྷ  */

foo before_\u0F43_after bar  // { dg-error "'before_.U00000f43_after' is not in NFC .-Werror=normalized=." }
/* { dg-begin-multiline-output "" }
 foo before_\u0F43_after bar
     ^~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

foo before_གྷ_after bar // { dg-error "'before_.U00000f43_after' is not in NFC .-Werror=normalized=." }
/* { dg-begin-multiline-output "" }
 foo before_<U+0F43>_after bar
     ^~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
