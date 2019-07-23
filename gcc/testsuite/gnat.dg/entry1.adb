--  { dg-do compile }
--  { dg-options "-gnateF" }

PACKAGE BODY Entry1 IS

   PROTECTED TYPE key_buffer IS

      PROCEDURE clear;

      ENTRY incr;
      ENTRY put (val : IN Natural);
      ENTRY get (val : OUT Natural);

   PRIVATE

      -- Stores Key states (key state controller)
      -- purpose: exclusive access
      max_len : Natural := 10;

      cnt : Natural := 0;

   END key_buffer;

   PROTECTED BODY key_buffer IS

      PROCEDURE clear IS
      BEGIN
         cnt := 0;
      END clear;

      ENTRY incr WHEN cnt < max_len IS
      BEGIN
         cnt := cnt + 1;
      END;

      ENTRY put (val : IN Natural) WHEN cnt < max_len IS
      BEGIN
         cnt := val;
      END put;

      ENTRY get (val : OUT Natural) WHEN cnt > 0 IS
      BEGIN
         val := cnt;
      END get;

   END key_buffer;

   my_buffer : key_buffer;

   FUNCTION pt2 (t : IN Float) RETURN Natural IS
      c : Natural;
      t2 : duration := duration (t);
   BEGIN
      SELECT
         my_buffer.get (c);
         RETURN c;
      OR
         DELAY t2;
         RETURN 0;
      END SELECT;
   END pt2;

   FUNCTION pt (t : IN Float) RETURN Natural IS
      c : Natural;
   BEGIN
      SELECT
         my_buffer.get (c);
         RETURN c;
      OR
         DELAY Duration (t);
         RETURN 0;
      END SELECT;
   END pt;

END Entry1;
