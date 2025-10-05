--  { dg-do compile }

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Allocator3 is

  package Queue_Interfaces is
    new Ada.Containers.Synchronized_Queue_Interfaces (Integer);

  package Synchronized_Queues is
    new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces);

  subtype Queue is Synchronized_Queues.Queue;

  type Access_Type is access all Queue;

  Q1 : Access_Type := new Queue;
  Q2 : Access_Type := new Queue;

begin
  null;
end;
