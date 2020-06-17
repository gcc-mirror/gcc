/* { dg-do compile } */
/* { dg-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */
/* { dg-enable-nn-line-numbers "" } */

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>

extern void body_of_program(void);

void custom_logger(const char *msg)
{
  fprintf(stderr, "LOG: %s", msg); /* { dg-warning "call to 'fprintf' from within signal handler" } */
}

static void int_handler(int signum)
{
  custom_logger("got signal");
}

static void register_handler ()
{
  signal(SIGINT, int_handler);
}

void test (void)
{
  register_handler ();
  body_of_program();
}

/* { dg-begin-multiline-output "" }
   NN |   fprintf(stderr, "LOG: %s", msg);
      |   ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  'test': events 1-2
    |
    |   NN | {
    |      | ^
    |      | |
    |      | (1) entering 'test'
    |   NN |   register_handler ();
    |      |   ~~~~~~~~~~~~~~~~~~~
    |      |   |
    |      |   (2) calling 'register_handler'
    |
    +--> 'register_handler': events 3-4
           |
           |   NN | {
           |      | ^
           |      | |
           |      | (3) entering 'register_handler'
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
           |   NN | {
           |      | ^
           |      | |
           |      | (6) entering 'int_handler'
           |   NN |   custom_logger("got signal");
           |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (7) calling 'custom_logger'
           |
           +--> 'custom_logger': events 8-9
                  |
                  |   NN | {
                  |      | ^
                  |      | |
                  |      | (8) entering 'custom_logger'
                  |   NN |   fprintf(stderr, "LOG: %s", msg);
                  |      |   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  |      |   |
                  |      |   (9) calling 'fprintf'
                  |
   { dg-end-multiline-output "" } */
