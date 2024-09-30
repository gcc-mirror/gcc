/* { dg-additional-options "-fcompare-debug" } */

/* PR target/116189 */

/* In the sh backend, we used to create insn in the path of rtx_costs.
   This means sometimes the max uid for insns would be different between
   debugging and non debugging which then would cause gcse's hashtable
   to have different number of slots which would cause a different walk
   for that hash table.  */

extern void ff(void);
extern short nn[8][4];
typedef unsigned short move_table[4];
extern signed long long ira_overall_cost;
extern signed long long ira_load_cost;
extern move_table *x_ira_register_move_cost[1];
struct move { struct move *next; };
unsigned short t;
void emit_move_list(struct move * list, int freq, unsigned char mode, int regno) {
  int cost;
  for (; list != 0; list = list->next)
  {
    ff();
    unsigned short aclass = t;
    cost = (nn)[mode][aclass] ;
    ira_load_cost = cost;
    cost = x_ira_register_move_cost[mode][aclass][aclass] * freq ;
    ira_overall_cost = cost;
  }
}
