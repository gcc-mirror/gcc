/* Try to process this explicitly as UTF-8.

   { dg-do preprocess }
   { dg-options "-finput-charset=UTF-8 -Winvalid-utf8 -fdiagnostics-format=sarif-file" } */

/* This comment intentionally contains non-UTF-8 bytes:
 *   €˜<unknown>€™ may be used uninitialized
 */

/* 
   { dg-final { verify-sarif-file } }

   Verify that we captured the expected warnings.

     { dg-final { scan-sarif-file "\"results\": \\\[" } }
       { dg-final { scan-sarif-file "\"level\": \"warning\"" } }
       { dg-final { scan-sarif-file "\"ruleId\": \"-Winvalid-utf8\"" } }
       { dg-final { scan-sarif-file "\"message\": " } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <98>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <80>"} } }
         { dg-final { scan-sarif-file {"text": "invalid UTF-8 character <99>"} } }
*/
