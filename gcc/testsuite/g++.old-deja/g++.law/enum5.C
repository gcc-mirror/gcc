// Build don't link: 
// GROUPS passed enums
// excess errors test - XFAIL *-*-*
// enum file
// From: Peter Samuelson <psamuels@osiris.ac.hmc.edu>
// Date:     Tue, 2 Nov 1993 23:44:34 -0800 (PST)
// Subject:  compiler error 192
// Message-ID: <199311030744.XAA23409@osiris.ac.hmc.edu>

#include <iostream.h>
#include <stdlib.h>
#include <string.h>

/* My program to generate D&D characters
*  10/25/93
*/

//prototypes
int rndshift();
int dice(int times, int sides);
void initialize(struct Crctr &character);
void cleararray(char thearray[24]);
void input(struct Crctr &character);
void rollscores(struct Crctr &character);
void output(struct Crctr &character);

int main()
{
  enum goopes
    {
      cleric, druid, fighter, knight, paladin, avenger, magicuser, thief,
      dwarf, halfelf, elf, halfling, mystic, ranger, scout, monster,
      immortal, special
    };

  enum alignments
    {
      lawful, neutral, chaotic
    };

  struct savingthrows
    {
      int psn_dth, wands, petr_paral, breath, rod_staff_spell;
    };

  struct abilities
    {
      int str, ite, wis, dex, con, cha;
    };
  struct valuables
    {
      char type[16];
      int value;
    };
  struct money
    {
      int pp, gp, ep, sp, cp;
      valuables gems[24], jewelry[24];
    };

  struct Crctr
    {
      Crctr() : goop(fighter) {}

      char dm[24], campaign[24], name[24], player[24];
      goopes goop;
      alignments alignment;
      int level, maxhit, hitpoints, ac;
      abitities scores;// ERROR - .*
      savingthrows saves;
      money wealth;
      int experience;
      float bonus;
    };
  
  Crctr character;

  cout << "The D&D Character generator!!" << endl;
  
  rndshift();

  initialize(character);// ERROR - .*
  input(character);
  rollscores(character);
  output(character);

  cout << "Thank you for using this program." << endl;
  cout << "Copyright 1993, Chase Enterprises, Inc." << endl;
  cout << "D&D and Dungeons and Dragons are registered trademarks" << endl;
  cout << "of TSR, Inc." << endl;
} //end of main


void initialize(Crctr &character)
{
  cleararray(character.dm);
  cleararray(character.campaign);
  cleararray(character.name);
  cleararray(character.player);
  character.level = 0;
  character.maxhit = 0;
  character.hitpoints = 0;
  character.ac = 9;
  character.experience = 0;
  character.bonus = 0.0;
} //end of initialize

void cleararray(char thearray[])
{
  for (int i = 0 ; i < 24 ; i++)
    thearray[i] = 0;
} //end of cleararray

void input(Crctr &character)
{
  cout << "Please input name of the Dungeon Master." << endl;
  cin  >> character.dm;
  cout << endl << "Pleas input the name of the campaign." << endl;
  cin  >> character.campaign;
  cout << endl << "Please input the name of the character." << endl;
  cin  >> character.name;
  cout << endl << "Please input player name." << endl;
  cin  >> character.player;
  cout << endl << "Enter desired goop." << endl;
  cin  >> character.goop;
  cout << endl << "Enter desired alignment." << endl;
} //end of input

void rollscores(Crctr &character)
{
  character.scores.str = dice(3,6);
  character.scores.ite = dice(3,6);
  character.scores.wis = dice(3,6);
  character.scores.dex = dice(3,6);
  character.scores.con = dice(3,6);
  character.scores.cha = dice(3,6);
  character.level = 1;

  switch (character.goop)
    {
    case fighter:
    case dwarf:
    case half-elf:
      character.maxhit = dice(1,8);
      character.hitpoints = character.maxhit;
      break;
    case cleric:
    case elf:
    case halfling:
    case mystic:
    case scout:
      character.maxhit = dice(1,6);
      character.hitpoints = character.maxhit;
      break;
    case magic-user:
    case thief:
      character.maxhit = dice(1,4);
      character.hitpoints = character.maxhit;
      break;
    default:
      character.maxhit = 1;
      character.hitpoints = character.maxhit;
      break;
    }
} //end of rollscores


void output(Crctr &character)
{
  cout << "Your character is:" << endl;
  cout << "Your DM:\t" << character.dm << "\t\t\t" << "Campaign: "
       << character.campaign << endl;
  cout << "Player's name:\t" << character.player << endl;
  cout << "Character Name:\t" << character.name << endl;
  cout << "goop:\t" << character.goop << "\t\t\t" << "Alignment:\t"
       << character.alignment << endl;
  cout << "level:\t" << character.level << endl;
  cout << "Max. HP:\t" << character.maxhit << "\t\t\t" << "Current HP:\t"
       << character.hitpoints << endl;
  cout << "Abilities" << "\t\t\t" << "Saves" << endl;
  cout << endl;
  cout << "Str:\t" << character.scores.str << "\t\t" << "Poison/Death Ray:\t"
       << character.saves.psn_dth << endl;
  cout << "Int:\t" << character.scores.ite << "\t\t" << "Wands:           \t"
       << character.saves.wands << endl;
  cout << "Wis:\t" << character.scores.wis << "\t\t" << "Petrification" 
       << "/Paralysis" << character.saves.petr_paral << endl;

/*      abitities scores;
      savingthrows saves;
      money wealth;
      int experience;
      float bonus;
*/
}


int rndshift()
{
//Chase Tsang 10/25/93
//cheap way to shift the random number generator sequence
//because I can't change the randomseed
//requires <stdlib.h> and <iostream.h>

int shiftnumber;
int date;
int luckynumber;

cout << "Please input the date in 8 digits, no spaces, no other" << endl;
cout << "characters. (ex 10041974 for October 4, 1974)" << endl;
cin >> date;
cout << "Please input your lucky number for today (7 or less" << endl;
cout << "digits, and positive integer, please)" << endl;
cin >> luckynumber;

shiftnumber = date / luckynumber;

for (int i = 0; i < shiftnumber; i++)
        rand();
return 0;
}



int dice(int times, int sides)
{
  //dice rolling function, Chase Tsang 10/25/93
  //requires #include <stdlib.h>

  int total = 0;

  for (int i = 0; i < times; i++)
    total = total + (rand() % sides) + 1;

  return total;
}
