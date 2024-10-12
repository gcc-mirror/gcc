/* PR c/68833 */
/* { dg-do preprocess } */
/* { dg-options "-Werror=normalized" } */

\u0F43  // { dg-error "'.U00000f43' is not in NFC .-Werror=normalized=." }

/* { dg-prune-output "treated as errors" } */
