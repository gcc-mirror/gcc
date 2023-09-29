/* Verify how paths are printed for signal-handler diagnostics.  */

/* { dg-options "-fanalyzer -fdiagnostics-show-line-numbers -fdiagnostics-path-format=inline-events -fdiagnostics-show-caret" } */
/* { dg-enable-nn-line-numbers "" } */
/* { dg-require-effective-target signal } */

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>

extern void body_of_program(void);

void custom_logger(const char *msg)
{
  fprintf(stderr, "LOG: %s", msg); /* { dg-warning "call to 'fprintf' from within signal handler" "" { target c } } */
  /* { dg-warning "call to 'int fprintf\\(FILE\\*, const char\\*, ...\\)' from within signal handler" "" { target c++ } .-1 } */
}

static void int_handler(int signum)
{
  custom_logger("got signal");
}

static void __analyzer_register_handler ()
{
  signal(SIGINT, int_handler);
}

void test (void)
{
  __analyzer_register_handler ();
  body_of_program();
}

/* "call to 'fprintf' from within signal handler [CWE-479]".  */
/* { dg-begin-multiline-output "" }
   NN |   fprintf(stderr, "LOG: %s", msg);
      |   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  'test': events 1-2
    |
    |   NN | void test (void)
    |      |      ^~~~
    |      |      |
    |      |      (1) entry to 'test'
    |   NN | {
    |   NN |   __analyzer_register_handler ();
    |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (2) calling '__analyzer_register_handler' from 'test'
    |
    +--> '__analyzer_register_handler': events 3-4
           |
           |   NN | static void __analyzer_register_handler ()
           |      |             ^~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |             |
           |      |             (3) entry to '__analyzer_register_handler'
           |   NN | {
           |   NN |   signal(SIGINT, int_handler);
           |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (4) registering 'int_handler' as signal handler
           |
  event 5
    |
    |cc1:
    | (5): later on, when the signal is delivered to the process
    |
    +--> 'int_handler': events 6-7
           |
           |   NN | static void int_handler(int signum)
           |      |             ^~~~~~~~~~~
           |      |             |
           |      |             (6) entry to 'int_handler'
           |   NN | {
           |   NN |   custom_logger("got signal");
           |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (7) calling 'custom_logger' from 'int_handler'
           |
           +--> 'custom_logger': events 8-9
                  |
                  |   NN | void custom_logger(const char *msg)
                  |      |      ^~~~~~~~~~~~~
                  |      |      |
                  |      |      (8) entry to 'custom_logger'
                  |   NN | {
                  |   NN |   fprintf(stderr, "LOG: %s", msg);
                  |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  |      |   |
                  |      |   (9) call to 'fprintf' from within signal handler
                  |
  { dg-end-multiline-output "" { target c } } */
/* { dg-begin-multiline-output "" }
   NN |   fprintf(stderr, "LOG: %s", msg);
      |   ~~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~
  'void test()': events 1-2
    |
    |   NN | void test (void)
    |      |      ^~~~
    |      |      |
    |      |      (1) entry to 'test'
    |   NN | {
    |   NN |   __analyzer_register_handler ();
    |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    |      |                               |
    |      |                               (2) calling '__analyzer_register_handler' from 'test'
    |
    +--> 'void __analyzer_register_handler()': events 3-4
           |
           |   NN | static void __analyzer_register_handler ()
           |      |             ^~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |             |
           |      |             (3) entry to '__analyzer_register_handler'
           |   NN | {
           |   NN |   signal(SIGINT, int_handler);
           |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |         |
           |      |         (4) registering 'void int_handler(int)' as signal handler
           |
  event 5
    |
    |cc1plus:
    | (5): later on, when the signal is delivered to the process
    |
    +--> 'void int_handler(int)': events 6-7
           |
           |   NN | static void int_handler(int signum)
           |      |             ^~~~~~~~~~~
           |      |             |
           |      |             (6) entry to 'int_handler'
           |   NN | {
           |   NN |   custom_logger("got signal");
           |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |                |
           |      |                (7) calling 'custom_logger' from 'int_handler'
           |
           +--> 'void custom_logger(const char*)': events 8-9
                  |
                  |   NN | void custom_logger(const char *msg)
                  |      |      ^~~~~~~~~~~~~
                  |      |      |
                  |      |      (8) entry to 'custom_logger'
                  |   NN | {
                  |   NN |   fprintf(stderr, "LOG: %s", msg);
                  |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  |      |          |
                  |      |          (9) call to 'int fprintf(FILE*, const char*, ...)' from within signal handler
                  |
  { dg-end-multiline-output "" { target c++ } } */
