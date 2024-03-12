/* { dg-do compile } */
/* { dg-options "-fdiagnostics-path-format=inline-events -fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */
/* { dg-enable-nn-line-numbers "" } */

extern void acquire_lock_a(void);
extern void acquire_lock_b(void);

void foo ()
{
  acquire_lock_a ();
  acquire_lock_b ();
}

void bar ()
{
  acquire_lock_b ();
  acquire_lock_a (); /* { dg-warning "deadlock due to inconsistent lock acquisition order" } */
}

/* { dg-begin-multiline-output "" }
   NN |   acquire_lock_a ();
      |   ^~~~~~~~~~~~~~~~~
Thread: 'Thread 1'
  'foo': event 1
    |
    |   NN | {
    |      | ^
    |      | |
    |      | (1) entering 'foo'
    |
    +--> 'foo': event 2
           |
           |   NN |   acquire_lock_a ();
           |      |   ^~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (2) lock a is now held by thread 1
           |

Thread: 'Thread 2'
  'bar': event 3
    |
    |   NN | {
    |      | ^
    |      | |
    |      | (3) entering 'bar'
    |
    +--> 'bar': event 4
           |
           |   NN |   acquire_lock_b ();
           |      |   ^~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (4) lock b is now held by thread 2
           |

Thread: 'Thread 1'
         'foo': event 5
           |
           |   NN |   acquire_lock_b ();
           |      |   ^~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (5) deadlocked due to waiting for lock b in thread 1...
           |

Thread: 'Thread 2'
         'bar': event 6
           |
           |   NN |   acquire_lock_a ();
           |      |   ^~~~~~~~~~~~~~~~~
           |      |   |
           |      |   (6) ...whilst waiting for lock a in thread 2
           |
     { dg-end-multiline-output "" } */
