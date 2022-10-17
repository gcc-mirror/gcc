/* Verify that #pragma GCC diagnostic down the inlining stack suppresses
   a warning that would otherwise be issued for inlined calls higher up
   the inlining stack.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds" } */

extern void* memset (void*, int, __SIZE_TYPE__);

static void warn0 (int *p)
{
  memset (p, __LINE__, 3);    // { dg-warning "\\\[-Wstringop-overflow" }
}

static void warn1 (int *p)
{
  warn0 (p + 1);
}

static void warn2 (int *p)
{
  warn1 (p + 1);
}

int a2[2];                    // { dg-message "at offset 12 into destination object 'a2' of size 8" }

void warn3 (void)
{
  warn2 (a2 + 1);
}


// Verify suppression at the innermost frame of the inlining stack.

static void ignore0 (int *p)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
  memset (p, __LINE__, 3);
#pragma GCC diagnostic pop
}

static void nowarn1_ignore0 (int *p)
{
  ignore0 (p + 1);
}

static void nowarn2_ignore0 (int *p)
{
  nowarn1_ignore0 (p + 1);
}

int b2[2];

void nowarn3_ignore0 (void)
{
  nowarn2_ignore0 (b2 + 1);
}


// Verify suppression at the second innermost frame of the inlining stack.

static void nowarn0_ignore1 (int *p)
{
  memset (p, __LINE__, 3);
}

static void ignore1 (int *p)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
  nowarn0_ignore1 (p + 1);
#pragma GCC diagnostic pop
}

void nowarn2_ignore1 (int *p)
{
  ignore1 (p + 1);
}

int c2[2];

void nowarn3_ignore1 (void)
{
  nowarn2_ignore1 (c2 + 1);
}


// Verify suppression at the third innermost frame of the inlining stack.

static void nowarn0_ignore2 (int *p)
{
  memset (p, __LINE__, 3);
}

static void nowarn1_ignore2 (int *p)
{
  nowarn0_ignore2 (p + 1);
}

static void ignore2 (int *p)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
  nowarn1_ignore2 (p + 1);
#pragma GCC diagnostic pop
}

int d2[2];

void nowarn3_ignore2 (void)
{
  ignore2 (c2 + 1);
}


// Verify suppression at the outermost frame of the inlining stack.

static void nowarn0_ignore3 (int *p)
{
  memset (p, __LINE__, 3);
}

static void nowarn1_ignore3 (int *p)
{
  nowarn0_ignore3 (p + 1);
}

static void nowarn2_ignore3 (int *p)
{
  nowarn1_ignore3 (p + 1);
}

int e2[2];

void ignore3 (void)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-overflow"
  nowarn2_ignore3 (e2 + 1);
#pragma GCC diagnostic pop
}
