typedef struct input {
        struct input *next;
} input_t;
static input_t *inputs = (input_t *)((void *)0);
void
RemoveInput(unsigned long id)
{
 input_t *ip;
 input_t *prev;
 while (1)
  if (ip == (input_t *)id)
   break;
 if (ip == (input_t *)((void *)0))
  return;
  prev->next = ip->next;
}
