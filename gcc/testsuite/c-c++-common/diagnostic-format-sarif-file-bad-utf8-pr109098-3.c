/* Adapted from cpp/Winvalid-utf8-1.c

   P2295R6 - Support for UTF-8 as a portable source file encoding
   This test intentionally contains various byte sequences which are not valid UTF-8
   { dg-do preprocess }
   { dg-options "-finput-charset=UTF-8 -Winvalid-utf8 -fdiagnostics-format=sarif-file" } */

// aÂ€ß¿à €íŸ¿î€€ğ€€ô¿¿a
// a€a
// a¿a
// aÀa
// aÁa
// aõa
// aÿa
// aÂa
// aàa
// aà€¿a
// aàŸ€a
// aà¿a
// aì€a
// aí €a
// ağ€€€a
// ağ¿¿a
// aô€€a
// aı¿¿¿¿¿
/* aÂ€ß¿à €íŸ¿î€€ğ€€ô¿¿a */
/* a€a */
/* a¿a */
/* aÀa */
/* aÁa */
/* aõa */
/* aÿa */
/* aÂa */
/* aàa */
/* aà€¿a */
/* aàŸ€a */
/* aà¿a */
/* aì€a */
/* aí €a */
/* ağ€€€a */
/* ağ¿¿a */
/* aô€€a */
/* aı¿¿¿¿¿a */



/* Verify that we generate a valid UTF-8 .sarif file.

     { dg-final { verify-sarif-file } }

   Verify that we captured the expected warnings.

     { dg-final { scan-sarif-file "\"results\": \\\[" } }
       { dg-final { scan-sarif-file "\"level\": \"warning\"" } }
       { dg-final { scan-sarif-file "\"ruleId\": \"-Winvalid-utf8\"" } }
       { dg-final { scan-sarif-file "\"message\": " } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <c0>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <c1>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <f5>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <ff>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <c2>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <e0>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <e0><80><bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <e0><9f><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <e0><bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <ec><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <ed><a0><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <f0><80><80><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <f0><8f><bf><bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <f4><90><80><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <fd><bf><bf><bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <c0>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <c1>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <f5>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <ff>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <c2>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <e0>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <e0><80><bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <e0><9f><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <e0><bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <ec><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <ed><a0><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <f0><80><80><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <f0><8f><bf><bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <f4><90><80><80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <fd><bf><bf><bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <bf>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <bf>"} } }
*/
