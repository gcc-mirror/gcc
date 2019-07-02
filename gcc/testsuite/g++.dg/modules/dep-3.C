// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -MD -Mno-modules" }

module m:part;
// { dg-module-cmi !m:part }

// All The Backslashes!
// { dg-final { scan-file-not dep-3.d {part\.gcm} } }
// { dg-final { scan-file-not dep-3.d {part\.c\+\+m} } }
