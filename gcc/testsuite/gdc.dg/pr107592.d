// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=107592
// { dg-do compile }

void test107592(Things...)(Things things)
{
    label:
    foreach (thing; things)
    {
        continue label;
    }
}

alias a107592 = test107592!(string);
