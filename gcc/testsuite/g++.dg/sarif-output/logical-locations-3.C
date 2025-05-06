/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

/* Verify that we handle deeply nested logical locations
   (PR 116176).  */

#define NS_OPEN(SUFFIX) namespace ns_##SUFFIX {
#define NS_CLOSE   }

#define NS_OPEN_10(SUFFIX) \
  NS_OPEN(SUFFIX##0) \
  NS_OPEN(SUFFIX##1) \
  NS_OPEN(SUFFIX##2) \
  NS_OPEN(SUFFIX##3) \
  NS_OPEN(SUFFIX##4) \
  NS_OPEN(SUFFIX##5) \
  NS_OPEN(SUFFIX##6) \
  NS_OPEN(SUFFIX##7) \
  NS_OPEN(SUFFIX##8) \
  NS_OPEN(SUFFIX##9)

#define NS_CLOSE_10 \
  NS_CLOSE \
  NS_CLOSE \
  NS_CLOSE \
  NS_CLOSE \
  NS_CLOSE \
  NS_CLOSE \
  NS_CLOSE \
  NS_CLOSE \
  NS_CLOSE \
  NS_CLOSE

#define NS_OPEN_100(SUFFIX) \
  NS_OPEN_10(SUFFIX##0) \
  NS_OPEN_10(SUFFIX##1) \
  NS_OPEN_10(SUFFIX##2) \
  NS_OPEN_10(SUFFIX##3) \
  NS_OPEN_10(SUFFIX##4) \
  NS_OPEN_10(SUFFIX##5) \
  NS_OPEN_10(SUFFIX##6) \
  NS_OPEN_10(SUFFIX##7) \
  NS_OPEN_10(SUFFIX##8) \
  NS_OPEN_10(SUFFIX##9)

#define NS_CLOSE_100 \
  NS_CLOSE_10 \
  NS_CLOSE_10 \
  NS_CLOSE_10 \
  NS_CLOSE_10 \
  NS_CLOSE_10 \
  NS_CLOSE_10 \
  NS_CLOSE_10 \
  NS_CLOSE_10 \
  NS_CLOSE_10 \
  NS_CLOSE_10

#define NS_OPEN_200 \
  NS_OPEN_100(a) \
  NS_OPEN_100(b)

#define NS_CLOSE_200 \
  NS_CLOSE_100 \
  NS_CLOSE_100

NS_OPEN_200

void return_from_void ()
{
  return 0;
}

NS_CLOSE_200

/* We expect a failing compile due to the error, but the use of
   -fdiagnostics-format=sarif-file means there should be no output to stderr.
   DejaGnu injects this message; ignore it:
   { dg-prune-output "exit status is 1" } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest logical-locations-3.C "logical-locations-3.py" } } */
